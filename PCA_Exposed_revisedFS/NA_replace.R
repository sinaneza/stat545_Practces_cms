rm(list = ls())

library(tidyverse)
library(stringr)
library(forcats)
library(purrr)
library(listviewer)


A <- tibble(x= c("a", "a", "b", "b", "c","c", "d", "d", "d", "d"),
						y = c(NA, NA, 2, NA, 4, 7, 6, NA, 8, 12))

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
	} else {
		return(rep(mean(x,na.rm = TRUE), length(x[is.na(x)])))
	}
}

repeat_disturbed_mean(c(72,68, NA))

(A_G_nested2 <- A_G_nested %>%
	group_by(x) %>% 
	mutate(replacement = (map(map(data, "y"), repeat_disturbed_mean))))

A_G_nested2$replacement
jsonedit(A_G_nested2$replacement, mode = "view")
A_G_nested2$data
Replace_Na <- function(a,b) replace_na(a, list (y = b))

(y_n<- Replace_Na(A_G_nested2$data[[4]], A_G_nested2$replacement[[4]]))

A_G_nested2$data
(A_G_nested3 <- A_G_nested2 %>%
	mutate(y_n = map2(data,replacement, Replace_Na))) %>% 
	unnest(y_n)
A_G_nested3$y_n[[4]]

A_G_nested2$replacement

A_G_nested2 %>%
	with(map(replacement, class))
class(map(A_G_nested2$data,"y")[[1]])
class(A_G_nested2$replacement)[[1]]
