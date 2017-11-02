rm(list = ls())
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)

exposed30 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												sheet = 1)

exposed90 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												sheet = 2)

exposed180 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 3)

exposed270 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 4)

exposed360 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 5)

(Exposed_list <- list(exposed30 = exposed30,
											exposed90 = exposed90,
											exposed180 = exposed180,
											exposed270 = exposed270,
											exposed360 = exposed360))


(Exposed <- bind_rows(bind_rows(Exposed_list$exposed30,
																Exposed_list$exposed90,
																Exposed_list$exposed180,
																Exposed_list$exposed270,
																Exposed_list$exposed360)) %>% 
		mutate(Cure = as_factor(Cure),
					 Material = as_factor(Material),
					 Coating = as_factor(Coating),
					 crit_3 = str_c(Material, Coating,
					 							 sep = "_"))
)

Mutate_Vars <- function(data, vars, ...){
	library(tidyverse)
	library(stringr)
	if (is.null(data$criteria)) {
		data <- mutate(data, criteria = data[[vars[[1]]]], ...)
	}
	if (length(vars)>1){
		for (i in 1:(length(vars)-1)) {
			data <- mutate(data, 
										 criteria = str_c(criteria, data[[vars[[i+1]]]],
										 								 sep = "_"), ...)
		}
	}
	return(data)
}

(G <- Mutate_Vars(Exposed, c("Material", "Coating")))
G$criteria



a <- c(0, 400, 800)
str_c(as.character(a),c("px", "px", "px"), sep = "")

