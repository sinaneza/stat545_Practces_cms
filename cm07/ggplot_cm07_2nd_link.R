rm(list = ls())
library(tidyverse)


library(gapminder)

gapminder %>% 
	ggplot(aes(x = year, y = lifeExp)) +
	geom_point(alpha = 1/4, size = 4)


gapminder %>%
	ggplot(aes(x = year, y = lifeExp))+
	geom_jitter()

gapminder %>%
	ggplot(aes(x = continent, y = lifeExp))+
	geom_jitter(alpha = 1/4, position = position_jitter(width = 0.1, height = 0))

gapminder %>%
	ggplot(aes(x = continent, y = lifeExp)) +
	geom_boxplot()


gapminder %>%
	ggplot(aes(x = continent, y = lifeExp)) +
	geom_boxplot(outlier.colour = "hotpink") +
	geom_jitter(alpha = 1/4, position = position_jitter(width =0.1, height = 0))

gapminder %>%
	ggplot(aes(x = continent, y = lifeExp)) +
	geom_jitter(alpha = 1/4, position = position_jitter(width = 0.1, height = 0)) +
	stat_summary(fun.y = median, colour = "red", geom = "point", size = 5) + 
	stat_summary(fun.y = mean, colour = "blue", geom = "point", size = 2)

gapminder %>%
	mutate(continent = reorder(continent, lifeExp)) %>% 
	ggplot(aes(x = continent, y = lifeExp)) +
	geom_jitter (alpha =1/4, position = position_jitter(width = 0.1 , height = 0)) +
	stat_summary(fun.y = median, geom = "point", colour = "red", size = 5)


gapminder %>%
	ggplot(aes(x = reorder(continent, lifeExp), y = lifeExp)) +
	geom_jitter (alpha =1/4, position = position_jitter(width = 0.1 , height = 0)) +
	stat_summary(fun.y = median, geom = "point", colour = "red", size = 5)
