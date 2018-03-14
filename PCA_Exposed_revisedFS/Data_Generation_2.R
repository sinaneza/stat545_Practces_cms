rm(list = ls())
setwd("E:/R_Projects/stat545_Practces_cms/shiny_comp_Exposed_revisedFS")
"E:/R_Projects/stat545_Practces_cms"
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(forcats)
exposed0 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
											 sheet = 1)

exposed30 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
												sheet = 2)

exposed90 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
												sheet = 3)

exposed180 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
												 sheet = 4)

exposed270 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
												 sheet = 5)

exposed360 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_RevisedFS_2.xlsx",
												 sheet = 6)

(Exposed_list <- list(exposed0 = exposed0,
											exposed30 = exposed30,
											exposed90 = exposed90,
											exposed180 = exposed180,
											exposed270 = exposed270,
											exposed360 = exposed360))


(Exposed <- bind_rows(bind_rows(Exposed_list$exposed0,
																Exposed_list$exposed30,
																Exposed_list$exposed90,
																Exposed_list$exposed180,
																Exposed_list$exposed270,
																Exposed_list$exposed360)) %>% 
		mutate(Material = as.factor(Material),
					 Coating = str_replace(Coating, "PR", "NG"),
					 Coating = as.factor(Coating),
					 Cure = as.factor(Cure),
					 crit_3 = str_c(Material, Coating,
					 							 sep = "_")))
Exposed$Material
Exposed$Coating
Exposed$Cure

Exposed %>% 
	filter(Time>90) %>% 
	View

Time_tibble <- tibble(Time2 = seq(0,5,1), Time = c(0, 30,
																									 90, 180,
																									 270, 360))

Exposed <- inner_join(Exposed, Time_tibble)
Exposed$Time2


(Exposed <- Exposed %>% 
		arrange(Time, Material, Coating, Cure) %>% 
		group_by(Material, Coating, Cure, Time) %>% 
		mutate(R_mean = mean (Roughness, na.rm = TRUE),
					 H_mean = mean (Hardness, na.rm = TRUE),
					 F_mean = mean (Max_Flexural_Stress, na.rm = TRUE)) %>% 
		ungroup() %>% 
		group_by(Material, Coating, Cure) %>% 
		mutate(GainedR_mean = (R_mean - first(R_mean))/first(R_mean),
					 GainedH_mean = (H_mean - first(H_mean))/first(H_mean),
					 GainedF_mean = (F_mean - first(F_mean))/first(F_mean)
		) %>% 
		ungroup()
)

Exposed %>%
	filter(Material == "W", 
				 Coating == "NG",
				 Cure == "FC") %>% 
	select(Time, Material, Coating, Cure, 
				 Roughness, R_mean, GainedR_mean,
				 Hardness, H_mean, GainedH_mean,
				 Max_Flexural_Stress, F_mean, GainedF_mean) %>% 
	View()

saveRDS(Exposed, "Exposed_revisedFS_2.rds")

(Type <- Exposed %>% 
		group_by(Material, Coating, Cure) %>% 
		summarize() %>% 
		ungroup()) 

names(Type) <- c("material", "coating", "cure")

(colour_Material <- tibble(material = unique(Type$material),
													 Material = c("#f00000", "#000000", "#0000ff")))

(colour_Coating <- tibble(coating = unique(Type$coating),
													Coating = c("#ff6600", "#00cc00")))

((colour_Cure <- tibble(cure = unique(Type$cure),
												Cure = c("#f00000", "#0000ff"))))

(colour_Material_Coating <- Type %>%
		group_by(material, coating) %>% 
		summarise() %>% 
		ungroup() %>% 
		mutate(Material_Coating = 
					 	c("#b30000", "#ff6333", "#000000","#000099", "#9900cc")))

(colour_Material_Cure <- Type %>%
		group_by(material, cure) %>% 
		summarise() %>% 
		ungroup() %>% 
		mutate(Material_Cure = 
					 	c("#990000", "#ff3399", "#000000", "#663300", "#000066","#00e6e6")))

(colour_Coating_Cure <- Type %>%
		group_by(coating, cure) %>% 
		summarise() %>% 
		ungroup() %>% 
		mutate(Coating_Cure = c("#ff6600", "#ffcc00",
														"#009933", "#00e6e6")))

(colour_Material_Coating_Cure <- Type %>%
		mutate(Material_Coating_Cure = 
					 	c("#800000", "#e60000", "#ff5722", "#ffcc00",
					 		"#000000", "#663300",
					 		"#000066", "#3366ff", "#730099", "#ff00ff")))

(A <- inner_join(colour_Material, Type))
(B <- inner_join(colour_Coating, A))
(C <- inner_join(colour_Cure, B))
(D <- inner_join(colour_Material_Coating, C))
(E <- inner_join(colour_Material_Cure, D))
(G <- inner_join(colour_Coating_Cure, E))
(colour <- inner_join(colour_Material_Coating_Cure, G))
saveRDS(colour, "colour.rds")
