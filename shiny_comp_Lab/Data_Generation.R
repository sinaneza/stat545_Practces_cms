rm(list = ls())
setwd("E:/R_Projects/stat545_Practces_cms/shiny_comp_Lab")
"E:/R_Projects/stat545_Practces_cms"
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(forcats)
lab0 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
											 sheet = 1)

lab30 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
												sheet = 2)

lab90 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
												sheet = 3)

lab180 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
												 sheet = 4)

lab270 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
												 sheet = 5)

lab360 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/LabGroup/Lab_SummaryEliminated.xlsx",
												 sheet = 6)

(lab_list <- list(lab0 = lab0,
											lab30 = lab30,
											lab90 = lab90,
											lab180 = lab180,
											lab270 = lab270,
											lab360 = lab360))


lab_list$lab180

(lab <- bind_rows(bind_rows(lab_list$lab0,
																lab_list$lab30,
																lab_list$lab90,
																lab_list$lab180,
																lab_list$lab270,
																lab_list$lab360)) %>% 
		mutate(Material = as.factor(Material),
					 # Coating = str_replace(Coating, "PR", "NG"),
					 Coating = as.factor(Coating),
					 Cure = as.factor(Cure),
					 crit_3 = str_c(Material, Coating,
					 							 sep = "_")))

lab %>%
	filter(Material == "R") %>% 
	View

lab$Material
lab$Coating
lab$Cure

Time_tibble <- tibble(Time2 = seq(0,5,1), Time = c(0, 30,
																									 90, 180,
																									 270, 360))

lab <- inner_join(lab, Time_tibble)
lab$Time2


(lab <- lab %>% 
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

lab %>%
	filter(Material == "C", 
				 Coating == "GC",
				 Cure == "FC") %>% 
	select(Time, Material, Coating, Cure, 
				 Roughness, R_mean, GainedR_mean,
				 Hardness, H_mean, GainedH_mean,
				 Max_Flexural_Stress, F_mean, GainedF_mean) %>% 
	View()

saveRDS(lab, "lab.rds")

(Type <- lab %>% 
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
