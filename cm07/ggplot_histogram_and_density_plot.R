rm(list = ls())
library(gapminder)
library(tidyverse)


DG <- filter(gapminder, country=="Germany") %>% 
  ggplot(aes(x=lifeExp))+geom_histogram(binwidth = 5)
DG
filter(gapminder, country=="Germany") %>% 
  ggplot(aes(x=lifeExp)) +
	geom_density()
