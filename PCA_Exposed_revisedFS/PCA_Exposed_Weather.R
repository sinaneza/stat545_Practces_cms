rm(list = ls())




setwd("E:/R_Projects/stat545_Practces_cms/PCA_Exposed_revisedFS")

library(tidyverse)
library(stringr)
library(forcats)
library(purrr)
library(listviewer)



Mutate_VarCollapse_criteria <- function(data, collapsing_vars, ...){
	library(tidyverse)
	library(stringr)
	library(forcats)
	data <- mutate(data,
								 criteria = data[[collapsing_vars[[1]]]],
								 criteria = as.factor(criteria),
								 ...)
	if (length(collapsing_vars)>1){
		for (i in 1:(length(collapsing_vars)-1)) {
			data <- mutate(data,
										 criteria = str_c(criteria, data[[collapsing_vars[[i+1]]]],
										 								 sep = "_", ...),
										 criteria = as.factor(criteria),
										 ...)
		}
	}
	return(data)
}

Mutate_VarCollapse_facet <- function(data, collapsing_vars, ...){
	library(tidyverse)
	library(stringr)
	library(forcats)
	data <- mutate(data,
								 facet = data[[collapsing_vars[[1]]]],
								 facet = as.factor(facet),
								 ...)
	if (length(collapsing_vars)>1){
		for (i in 1:(length(collapsing_vars)-1)) {
			data <- mutate(data,
										 facet = str_c(facet, data[[collapsing_vars[[i+1]]]],
										 							sep = "_", ...),
										 facet = as.factor(facet),
										 ...)
		}
	}
	return(data)
}

(Exposed_new_complete <- readRDS("Exposed_RevisedFSNA_2.rds"))

MaterialNo <- tibble(Material = as.factor(c("W", "R", "C")),
										 Material_No = c(1,3,2))

CoatingNo <- tibble(Coating = as.factor(c("GC", "NG")),
										Coating_No = c(1,2))

CureNo <- tibble(Cure = as.factor(c("FC", "PC")),
								 Cure_No = c(1,2))


(Exposed_new_typequant <- Exposed_new_complete %>% 
	inner_join(MaterialNo) %>% 
	inner_join(CoatingNo) %>% 
	inner_join(CureNo))

(Weather_UBCO_UVRH <- readRDS("Weather_UBCO_UVRH.rds"))


(Exposed_Weather <- Weather_UBCO_UVRH %>% 
	select(c("Time", "precipitation","SnowLevel_cm",
					 "mean_temp", "mean_UV","mean_RH")) %>% 
	inner_join(Exposed_new_typequant))

# saveRDS(Exposed_Weather,"Exposed_Weather_revisedNAFS.rds")


# 1) Including Flexural Strength:

crit_vars <- c("Material","Coating", "Cure")
AnalCrit <- c("Material","Coating", "Cure")

colour_scheme <- str_c(AnalCrit, collapse = "_")
colour <- readRDS("colour.rds")

Exposed_Weather %>% 
	View

(FS <- Exposed_Weather %>% 
	filter(!is.na(Max_Flexural_Stress)))

# a) defining colouring criteria:

FS_crit <- FS %>% 
	Mutate_VarCollapse_criteria(collapsing_vars = AnalCrit)

# b) defining filtering criteria:

FS_filtered <- FS_crit %>% 
	filter(Material == "W") %>% 
	# filter(Material == "W",
	# 			 Coating == "GC",
	# 			 Cure == "PC") %>%
	mutate(Material = fct_drop(Material),
				 Coating = fct_drop(Coating),
				 Cure = fct_drop(Cure),
				 criteria = fct_drop(criteria))

FS_filtered$Material
FS_filtered$Coating
FS_filtered$Cure
FS_filtered$criteria
# c) changing the qualitative variables to quantitative

(FS_QualtoQuant <- FS_filtered %>% 
	mutate(Material = Material_No,
				 Coating = Coating_No,
				 Cure = Cure_No) %>% 
	select(-Material_No, -Coating_No, -Cure_No))

FS_QualtoQuant$criteria

# d) Selcecting variables, quantitative variables, to be included in analysis
# #and then applying PCA

# #Note: If the selected filtering criteria is just limited to one sub-category,
# #That variable should get eliminated from analysis 
# #(Constant variables cause issue in PCA algorithm)

(FS_pca <- FS_QualtoQuant %>% 
		select(- Material, -Coating, -Cure) %>% 
		select("Time",
					 # "Coating", "Cure",
					 "Roughness", "Hardness", "SG", "Max_Flexural_Stress",
					 "mean_temp", "mean_UV", "precipitation", "SnowLevel_cm") %>% 
		rename(Flexural = Max_Flexural_Stress,
					 temp = mean_temp,
					 UV = mean_UV,
					 Snow = SnowLevel_cm) %>% 
	prcomp(center = TRUE ,scale = TRUE, retx = TRUE))


# e) Score and Loading Plots:

# When Flexural Strength is included in model, pure resin samples are neglected.
colour0 <- colour %>% 
	filter(material != "R") %>%
	filter(material %in% unique(FS_filtered[["Material"]])) %>% 
	filter(coating %in% unique(FS_filtered[["Coating"]])) %>% 
	filter(cure %in% unique(FS_filtered[["Cure"]])) %>% 
	mutate(material = fct_drop(material),
				 coating = fct_drop(coating),
				 cure = fct_drop(cure))
Colour <- unique(colour0[[colour_scheme]])
# 

library(ggfortify)
(plot_FS <- autoplot(FS_pca, x= 1, y= 2,
										 data = FS_QualtoQuant, colour = "criteria",
										 loadings = TRUE, loadings.label = TRUE,loadings.colour = "#000000",
										 loadings.label.colour = "#000000",
										 # frame = TRUE, frame.type = 'norm',
										 alpha =1/3,size = 3) +
		scale_colour_manual(values = Colour) +
		coord_fixed(ratio = 0.5) +
		scale_fill_manual(values = Colour) +
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")))

# ggsave("C:/additional/UBC/MENG_Papers/PCA/Weather/Whole360Days_FS/AllCategories/Whole.pdf", plot_FS, scale = 1)


# 2) Not considering Flexural Strngth (Considering the whole data)

crit_vars <- c("Material","Coating", "Cure")
AnalCrit <- c("Material","Coating", "Cure")

colour_scheme <- str_c(AnalCrit, collapse = "_")
colour <- readRDS("colour.rds")


(NoFS <- Exposed_Weather %>% 
		select(-Max_Flexural_Stress))

# a) defining colouring criteria:

NoFS_crit <- NoFS %>% 
	Mutate_VarCollapse_criteria(collapsing_vars = AnalCrit)

# b) defining filtering criteria:

(NoFS_filtered <- NoFS_crit %>% 
		filter(Material == "R"
					 # Coating == "NG",
					 # Cure == "PC"
					 ) %>%
	mutate(Material = fct_drop(Material),
				 Coating = fct_drop(Coating),
				 Cure = fct_drop(Cure)))

# c) changing the qualitative variables to quantitative

(NoFS_QualtoQuant <- NoFS_filtered %>% 
		mutate(Material = Material_No,
					 Coating = Coating_No,
					 Cure = Cure_No) %>% 
		select(-Material_No, -Coating_No, -Cure_No))


# d) Selcecting variables, quantitative variables, to be included in analysis
# #and then applying PCA

# #Note: If the selected filtering criteria is just limited to one sub-category,
# #That variable should get eliminated from analysis 
# #(Constant variables cause issue in PCA algorithm)

(NoFS_pca <- NoFS_QualtoQuant %>% 
		# select(- Material, -Coating, -Cure) %>% 
		select("Time",
					 # "Coating", "Cure",
					 "Roughness", "Hardness", "SG",
					 "mean_temp", "mean_UV", "precipitation", "SnowLevel_cm") %>% 
		rename(temp = mean_temp,
					 UV = mean_UV,
					 Snow = SnowLevel_cm) %>% 
		prcomp(center = TRUE ,scale = TRUE, retx = TRUE))

# e) Score and Loading Plots:

# When Flexural Strength is included in model, pure resin samples are neglected.
colour360 <- colour %>% 
	filter(material %in% unique(NoFS_filtered[["Material"]])) %>% 
	filter(coating %in% unique(NoFS_filtered[["Coating"]])) %>% 
	filter(cure %in% unique(NoFS_filtered[["Cure"]])) %>% 
	mutate(material = fct_drop(material),
				 coating = fct_drop(coating),
				 cure = fct_drop(cure))
Colour <- unique(colour360[[colour_scheme]])
# 

library(ggfortify)
(plot_360 <- autoplot(NoFS_pca, x= 1, y= 2,
										 data = NoFS_QualtoQuant, colour = "criteria",
										 loadings = TRUE, loadings.label = TRUE,loadings.colour = "#000000",
										 loadings.label.colour = "#000000",
										 # frame = TRUE, frame.type = 'norm',
										 alpha =1/3,size = 3) +
		scale_colour_manual(values = Colour) +
		coord_fixed()+
		scale_fill_manual(values = Colour) +
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")))

# ggsave("C:/additional/UBC/MENG_Papers/PCA/Weather/Whole360Days/Materialwise/R.png", plot_360, scale = 1)


