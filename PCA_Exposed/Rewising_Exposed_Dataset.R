
# Removing NAs on a test dataset
rm(list = ls())

library(tidyverse)
library(stringr)
library(forcats)
library(purrr)
library(listviewer)


(A <- tibble(x= c("a", "a", "b", "b", "c","c", "d", "d", "d", "d", "e", "e", "e"),
						 y = c(NA, NA, 2, NA, 4, 7, 6, NA, 8, 12, 10, 12, 13)))

(A_G_nested <- A %>% 
		group_by(x) %>% 
		nest())

repeat_mean <- function(x) {
	if (length(x[is.na(x)])>0) {
		return(rep(mean(x,na.rm = TRUE), length(x[is.na(x)])))
	}
}

repeat_disturbed_mean <- function(x) {
	if (length(x[!is.na(x)])>1) {
		return(abs(mean(x, na.rm = TRUE) + rnorm(length(x[is.na(x)])) * sd(x, na.rm = TRUE)))
	} else if (length(x[!is.na(x)]) == 1){
		return(rep(mean(x,na.rm = TRUE), length(x[is.na(x)])))
	} else {
		return(NA)
	}
}

repeat_disturbed_mean(c(72,68, NA))

(A_G_nested2 <- A_G_nested %>%
		group_by(x) %>% 
		mutate(replacement = (map(map(data, "y"), repeat_disturbed_mean))))

A_G_nested2$replacement
jsonedit(A_G_nested2$replacement, mode = "view")
A_G_nested2$data
jsonedit(A_G_nested2$data, mode = "view")



Replace_Na <- function(a,b) replace_na(a, list (y = b))

(y_n<- Replace_Na(A_G_nested2$data[[2]], A_G_nested2$replacement[[2]]))

A_G_nested2$data
(A_new <- A_G_nested2 %>%
		mutate(y_n = map2(data,replacement, Replace_Na))) %>% 
	unnest(y_n)

# Applying on Exposed Dataset

setwd("E:/R_Projects/stat545_Practces_cms/PCA")

Exposed <- readRDS("Exposed.rds")


repeat_disturbed_mean <- function(x) {
	if (length(x[!is.na(x)])>1) {
		return(abs(mean(x, na.rm = TRUE) + rnorm(length(x[is.na(x)])) * sd(x, na.rm = TRUE)))
	} else if (length(x[!is.na(x)]) == 1){
		return(rep(mean(x,na.rm = TRUE), length(x[is.na(x)])))
	} else {
		return(NA)
	}
}

Replace_Na_SG <- function(data,replacement) replace_na(data, list (SG = replacement))



# Exposed %>% 
# 	filter(!is.na(Roughness)) %>% 
# 	select(Time, Material, Coating, Cure,Roughness, SG) %>% 
# 	View()
Exposed_noRoughNA <- Exposed %>% 
	filter(!is.na(Roughness))

(Exposed_nested <- Exposed %>% 
		filter(!is.na(Roughness)) %>% 
		group_by(Time, Material, Coating, Cure) %>% 
		nest())

map(Exposed_nested$data, "SG")

(Exposed_newSG <- Exposed_nested %>% 
		mutate(replacement = (map(map(data, "SG"), repeat_disturbed_mean))) %>% 
		mutate(SG_n = map2(data, replacement, Replace_Na_SG)) %>% 
		unnest(SG_n))



(comparison <- data_frame(Time =Exposed_newSG$Time,
													Material = Exposed_newSG$Material,
													Coating = Exposed_newSG$Coating,
													Cure = Exposed_newSG$Cure,
													SG = Exposed_noRoughNA$SG,
													SG_new = Exposed_newSG$SG)) %>% 
	View()


Exposed %>% 
	filter(is.na(Roughness)) %>% 
	select(Time, Material, Coating, Cure,
				 Roughness, Hardness, Max_Flexural_Stress, SG) %>% 
	View()

Replace_Na_Hardness <- function(data,replacement) replace_na(data, list (Hardness = replacement))

(Exposed_newSGHardness <- Exposed_newSG %>% 
		group_by(Time, Material, Coating, Cure) %>%
		nest() %>% 
		mutate(replacement = 
					 	(map(map(data, "Hardness"), repeat_disturbed_mean))) %>% 
		mutate(Hardness_n = map2(data, replacement, Replace_Na_Hardness)) %>% 
		unnest(Hardness_n))


Comparison <- data_frame(Time =Exposed_newSGHardness$Time,
												 Material = Exposed_newSGHardness$Material,
												 Coating = Exposed_newSGHardness$Coating,
												 Cure = Exposed_newSGHardness$Cure,
												 Hardness = Exposed_noRoughNA$Hardness,
												 Hardness_new = Exposed_newSGHardness$Hardness,
												 SG = Exposed_noRoughNA$SG,
												 SG_new = Exposed_newSGHardness$SG
) %>% 
	View()

Replace_Na_Max_Flexural_Stress <- function(data, replacement) {
	replace_na(data, list(Max_Flexural_Stress = replacement))
}

(Exposed_new_complete <- Exposed_newSGHardness %>% 
		group_by(Time, Material, Coating, Cure) %>% 
		nest() %>% 
		mutate(replacement = map(map(data,"Max_Flexural_Stress"),
														 repeat_disturbed_mean)) %>% 
		mutate(Max_Flexural_Stress_n = 
					 	map2(data, replacement, Replace_Na_Max_Flexural_Stress)) %>% 
		unnest(Max_Flexural_Stress_n))

(Comparison <- tibble(Time =Exposed_new_complete$Time,
											Material = Exposed_new_complete$Material,
											Coating = Exposed_new_complete$Coating,
											Cure = Exposed_new_complete$Cure,
											Hardness = Exposed_noRoughNA$Hardness,
											Hardness_new = Exposed_new_complete$Hardness,
											SG = Exposed_noRoughNA$SG,
											SG_new = Exposed_new_complete$SG,
											Max_Flexural_Strength = Exposed_noRoughNA$Max_Flexural_Stress,
											Max_Flexural_Strength_new = Exposed_new_complete$Max_Flexural_Stress)) %>% 
	View()

# saveRDS(Exposed_new_complete, "Exposed_RewisedNAs.rds")
