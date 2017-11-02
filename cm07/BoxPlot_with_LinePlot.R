rm (list = ls())
library(gapminder)
library(tidyverse)

my_gap <- gapminder

gap_MeanMedian_Africa <- my_gap %>% 
	group_by(year, continent) %>% 
	summarize(mean_lifeExp = mean(lifeExp),
						median_lifeExp = median(lifeExp)) %>% 
	filter(continent == "Africa")

my_gap %>% 
	filter(continent == "Africa") %>% 
	ggplot(aes(x = as.factor(year), y = lifeExp)) +
	geom_boxplot() +
	geom_line(data = gap_MeanMedian_Africa,
						aes(x = as.factor(year), y = mean_lifeExp),
						group=1, colour="#ff3933") +
	geom_line(data = gap_MeanMedian_Africa,
						aes(x = as.factor(year), y = median_lifeExp),
						group=1, colour="#3342ff")
