rm(list = ls())
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(forcats)
# Defining the required functions:

Mutate_Vars <- function(data, vars, ...){
	library(tidyverse)
	library(stringr)
	# if (is.null(data$criteria)) {
	data <- mutate(data, criteria = data[[vars[[1]]]], ...)
	# }
	if (length(vars)>1){
		for (i in 1:(length(vars)-1)) {
			data <- mutate(data, 
										 criteria = str_c(criteria, data[[vars[[i+1]]]],
										 								 sep = "_"), ...)
		}
	}
	return(data)
}


# Reading The Required Data Set and variables

Exposed <- readRDS("Exposed.rds")
colour <- readRDS("colour.rds")
crit_vars <- c("Material", "Coating", "Cure")
