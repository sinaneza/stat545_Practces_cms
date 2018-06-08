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

# Exposed_new_complete %>% 
# 	View


MaterialNo <- tibble(Material = as.factor(c("W", "R", "C")),
												 Material_No = c(1,3,2))

CoatingNo <- tibble(Coating = as.factor(c("GC", "NG")),
												Coating_No = c(1,2))

CureNo <- tibble(Cure = as.factor(c("FC", "PC")),
										 Cure_No = c(1,2))
# Colouring Criteria Definition:
crit_vars <- c("Material","Coating", "Cure")
AnalCrit <- c("Material","Coating", "Cure")
FacetCrit <- c()

colour_scheme <- str_c(AnalCrit, collapse = "_")
colour <- readRDS("colour.rds")
# When Flexural Strength is included in model, pure resin samples are neglected.
colour0 <- colour %>% 
	filter(material != "R") %>% 
	mutate(material = fct_drop(material))
Colour <- unique(colour0[[colour_scheme]])
# 

Exposed_new_typequant <- inner_join(Exposed_new_complete, MaterialNo) %>% 
	inner_join(CoatingNo) %>% 
	inner_join(CureNo) %>% 
	Mutate_VarCollapse_criteria(collapsing_vars = AnalCrit) 
	# Mutate_VarCollapse_facet(collapsing_vars = FacetCrit)

(Exposed_new_pca <- Exposed_new_typequant %>%
		filter(Material != "R") %>% 
	select(Time, Material_No,Coating_No, Cure_No,
				 Roughness, Hardness, Max_Flexural_Stress, SG) %>% 
	rename(Material = Material_No,
				 Coating = Coating_No,
				 Cure = Cure_No) %>%
		rename(Flexural = Max_Flexural_Stress) %>% 
	# filter(!is.na(Max_Flexural_Stress)) %>%
	prcomp(center = TRUE ,scale = TRUE, retx = TRUE))

max(Exposed_new_pca$rotation["Coating",])
which.max(Exposed_new_pca$rotation["Roughness",])
which.max(Exposed_new_pca$rotation["Time",])

library(ggfortify)
(p <- autoplot(Exposed_new_pca, x= 1, y= 2,
				 data = filter(Exposed_new_typequant, !is.na(Max_Flexural_Stress)),
				 colour = "criteria", loadings = TRUE, loadings.label = TRUE,
				 # ellipse = TRUE,
				 frame = TRUE, frame.type = 'norm',
				 loadings.colour = "#000000", loadings.label.colour = "#000000",
				 alpha = 1/3, size = 3) +
	# facet_wrap(~ facet) +
	# coord_fixed()+
	scale_colour_manual(values = Colour) +
	scale_fill_manual(values = Colour) +
	theme(plot.subtitle = element_text(vjust = 1),
				plot.caption = element_text(vjust = 1),
				panel.grid.major = element_line(colour = "gray5",
																				linetype = "longdash"),
				panel.grid.minor = element_line(colour = "gray5",
																				linetype = "dotdash"),
				panel.background = element_rect(fill = "gray100"),
				axis.text = element_text(colour = "gray5")))

# ggsave("C:/additional/UBC/MENG_Papers/PCA/With_Flexural/Material_Coating_Cure_360.png",p, scale = 1)
# 
# Eliminating Flexural Strength


crit_vars <- c("Material", "Coating", "Cure")
AnalCrit <- c("Material", "Coating", "Cure")
FacetCrit <- c()

colour_scheme <- str_c(AnalCrit, collapse = "_")
Colour <- unique(colour[[colour_scheme]])

Exposed_new_typequant <- Exposed_new_typequant %>% 
	mutate(criteria = NULL) %>% 
	Mutate_VarCollapse_criteria(collapsing_vars = AnalCrit) 

(Exposed_new_pca2 <- Exposed_new_typequant %>% 
		select(Time, Material_No,Coating_No, Cure_No,
					 Roughness, Hardness, SG) %>% 
		rename(Material = Material_No,
					 Coating = Coating_No,
					 Cure = Cure_No) %>% 
		prcomp(center = TRUE ,scale = TRUE, retx = TRUE))

(p <- autoplot(Exposed_new_pca2, x= 1, y= 2,
							 data = Exposed_new_typequant,
							 colour = "criteria", loadings = TRUE, loadings.label = TRUE,
							 # ellipse = TRUE,
							 frame = TRUE, frame.type = 'norm',
							 loadings.colour = "#000000", loadings.label.colour = "#000000",
							 alpha = 1/3, size = 3) +
		# facet_wrap(~ facet) +
		# coord_fixed(ratio = 1) +
		scale_colour_manual(values = Colour) +
		scale_fill_manual(values = Colour) +
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")))

# ggsave("C:/additional/UBC/MENG_Papers/PCA/Without_Flexural/Material_Coating_Cure_2.png",p, scale = 1)

