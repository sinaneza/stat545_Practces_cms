rm(list = ls())
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(forcats)
# Defining the required functions:

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

# Mutate_Vars <- function(data, vars, crit_vars, ...){
# 	library(tidyverse)
# 	library(stringr)
# 	if (length (vars) == 1) {
# 		return(
# 			mutate(data, 
# 						 criteria = data[[vars[[1]]]],
# 						 facet = str_c(data[[crit_vars[!(crit_vars %in% vars)][[1]]]], 
# 						 							data[[crit_vars[!(crit_vars %in% vars)][[2]]]],
# 						 							sep = "_", ...), ...)
# 		)
# 	} else if (length(vars) == 2) {
# 		mutate(data,
# 					 criteria = str_c)
# 	} else if (length(vars) == 3) {
# 		
# 	}
# 	# # if (is.null(data$criteria)) {
# 	# data <- mutate(data, criteria = data[[vars[[1]]]], ...)
# 	# # }
# 	# if (length(vars)>1){
# 	# 	for (i in 1:(length(vars)-1)) {
# 	# 		data <- mutate(data, 
# 	# 									 criteria = str_c(criteria, data[[vars[[i+1]]]],
# 	# 									 								 sep = "_"),
# 	# 									 facet = data[[crit_vars[!(crit_vars %in% vars)][[1]]]],...)
# 	# 	}
# 	# } else if (length(vars) == 1) {
# 	# 	data <- mutate(data,
# 	# 								 facet = str_c(data[[crit_vars[!(crit_vars %in% vars)][[1]]]],
# 	# 								 							data[[crit_vars[!(crit_vars %in% vars)][[2]]]],
# 	# 								 							sep = "_", ...))
# 	# 	}
# 	return(data)
# }


# Reading The Required Data Set and variables

Lab_FS <- readRDS("lab_FS.rds")

colour <- readRDS("colour.rds")

crit_vars <- c("Material", "Coating", "Cure")
