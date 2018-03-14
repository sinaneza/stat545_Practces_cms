setwd("E:/R_Projects/stat545_Practces_cms/PCA")
rm(list = ls())
library(tidyverse)
library(stringr)
library(forcats)

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

Exposed <- readRDS("Exposed.rds")
colour <- readRDS("colour.rds")

crit_vars <- c("Material", "Coating", "Cure")
AnalCrit <- c("Material")
FacetCrit <- c("Coating","Cure")

colour_scheme <- str_c(AnalCrit, collapse = "_")

Colour <- unique(colour[[colour_scheme]])


E0 <- Exposed %>% 
	Mutate_VarCollapse_criteria(collapsing_vars = AnalCrit) %>% 
	Mutate_VarCollapse_facet(collapsing_vars = FacetCrit)
E0$criteria
E0$Material
E0$facet

E0_crit <- E0 %>% 
	group_by(Material, Coating, Cure, criteria) %>% 
	summarize() %>% 
	mutate(groups = criteria, criteria = NULL)

Exposed.pca <- E0 %>% 
	select(Time, Hardness, Roughness) %>%
	filter(!is.na(Hardness)) %>% 
	prcomp(center = TRUE , scale = TRUE)

types <- E0 %>%
	filter(!is.na(Hardness)) %>% 
	.$criteria

# library(ggbiplot)
# 
# # E0 %>%
# # 	filter(!is.na(Hardness)) %>%
# # 	select(Time, Hardness, Roughness, criteria) %>%
# # 	with(ggbiplot(Exposed.pca, obs.scale = 1, var.scale = 1,
# # 								groups = criteria, ellipse = TRUE,
# # 								circle = TRUE) +
# # 			 	scale_colour_manual(values = Colour) +
# # 			 	theme(plot.subtitle = element_text(vjust = 1),
# # 			 				plot.caption = element_text(vjust = 1),
# # 			 				panel.grid.major = element_line(colour = "gray5",
# # 			 																				linetype = "longdash"),
# # 			 				panel.grid.minor = element_line(colour = "gray5",
# # 			 																				linetype = "dotdash"),
# # 			 				panel.background = element_rect(fill = "gray100"),
# # 			 				axis.text = element_text(colour = "gray5")) 
# # 			 	# facet_wrap(~ Coating)
# # 			 	)
# 
# (plot <- ggbiplot(Exposed.pca, obs.scale = 1, var.scale = 1, 
# 				 groups = types, ellipse = TRUE, 
# 				 circle = TRUE) + 
# 		scale_colour_manual(values = Colour) +
# 		theme(plot.subtitle = element_text(vjust = 1),
# 				plot.caption = element_text(vjust = 1),
# 				panel.grid.major = element_line(colour = "gray5",
# 																				linetype = "longdash"),
# 				panel.grid.minor = element_line(colour = "gray5",
# 																				linetype = "dotdash"),
# 				panel.background = element_rect(fill = "gray100"),
# 				axis.text = element_text(colour = "gray5"))) 
# 	# facet_wrap(~ str_split_fixed(groups, pattern= "_", n = 2)[,1])
# 
# 
# 
# # (plot$data <- inner_join(plot$data, E0_crit))
# 
# plot + facet_wrap(~ Coating)
# 
# (str_split_fixed(plot$data$groups, pattern= "_", n = 2)[,1])
# dim(plot$data)


# install.packages("ggfortify")
library(ggfortify)
library(cluster)
EE0 <- E0 %>% 
	filter(!is.na(Hardness)) 

EE <- select(EE0,Time, Roughness, Hardness)

(p <- autoplot(prcomp(EE, center = TRUE, scale = TRUE), x= 1, y= 2,
							 data = EE0,
				 colour = "criteria", loadings = TRUE, loadings.label = TRUE,
				 frame = TRUE, frame.type = 'norm',
				 loadings.colour = "#000000", loadings.label.colour = "#000000",
				 alpha = 1/3, size = 3) +
		facet_wrap(~ facet) +
		coord_fixed()+
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

ggsave("C:/additional/UBC/MENG_Papers/PCA/Material_CoatingCureFacet.png",p, scale = 2)

p_d <- select(p$data, PC1, PC2, Time, Roughness,Hardness,
			 Material, Coating, Cure, criteria) 
	# mutate(Coating = as.character(Coating),
	# 				criteria = as.character(criteria))

# p_d %>% 
# 	with(autoplot(pam(p_d, 6), frame = TRUE, frame.type = 'norm', colour = "criteria") +
# 			 	aes(group = criteria))
	
autoplot(pam(p_d, 3), frame = TRUE, frame.type = 'norm', colour = "criteria",
				 groups = "criteria") +
	aes(group = "criteria", colour = "criteria") 
	
	# facet_wrap(~ Coating)
p_d$Coating
# , data = EE0, colour = "criteria"
EE2 <- select(EE0,Time, Roughness, Hardness, criteria)
autoplot(kmeans(EE,6), data = EE0,
				 colour = "criteria", loadings = TRUE, loadings.label = TRUE,
				 loadings.colour = "blue")


autoplot(fanny(EE,6), frame = TRUE)

autoplot(pam(EE, 6), frame = TRUE, frame.type = 'norm')


# Coding to find correlation of material, coating, cure with physical properties

x0 <- scale(EE, center = TRUE, scale = TRUE)
pca <- prcomp(EE, center = TRUE, scale = TRUE)
x1 <- pca$x
pca2 <- prcomp(EE, center = TRUE, scale = TRUE, retx = TRUE)
x2 <- pca2$x
x2==x1

diag(t(x2%*%t(pca$rotation))%*%(x2%*%t(pca$rotation)))
pca3 <- prcomp(EE, center = TRUE, scale = TRUE, rank=1, retx = TRUE)
x3 <- pca3$x
pca3$rotation
diag(t(x3%*%t(pca3$rotation))%*%(x3%*%t(pca3$rotation)))/diag(t(x0)%*%x0)

pca$sdev/sum(pca$sdev)


MaterialNo <- data_frame(Material = as.factor(c("W", "R", "C")),
												 Material_No = c(1,2,3))

CoatingNo <- data_frame(Coating = as.factor(c("GC", "NG")),
												Coating_No = c(1,2))

CureNo <- data_frame(Cure = as.factor(c("FC", "PC")),
										 Cure_No = c(1,2))

select(inner_join(E0,MaterialNo), Material , Material_No) %>% 
	print(n = Inf)
e01 <- inner_join(E0, MaterialNo)
e02 <- inner_join(e01, CoatingNo)
E_types <- inner_join(e02, CureNo)
E_types_pca <- e03 %>% 
	filter(!is.na(Hardness),
				 !is.na(Roughness)) %>% 
	select(Material_No, Coating_No, Cure_No, Time, Hardness, Roughness)
library(Matrix)
rankMatrix(as.matrix.data.frame(E_types_pca))
pca_model <- prcomp(E_types_pca, center = TRUE, scale = TRUE)
autoplot(pca_model, x= 1, y= 2,
				 loadings = TRUE, loadings.label = TRUE)
