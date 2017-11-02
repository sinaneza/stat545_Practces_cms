rm(list = ls())
library(tidyverse)
library(gapminder)

Year <- select (gapminder , year) %>% 
	unique()

Y <- Year$year[[4]]

gapminder %>%
	group_by(year, continent) %>% 
	summarize(mean_lifeExp = mean(lifeExp)) %>% 
	arrange(continent, year) %>% 
	group_by(continent) %>% 
	mutate(mean_lifeExp_delta = mean_lifeExp - lag(mean_lifeExp),
				 percentage_lifeExp_change = (mean_lifeExp_delta/mean_lifeExp)*100) %>% 
	filter(year>1952) %>% 
	print(n= Inf)
	filter(year == Y) %>% 
	ggplot(aes(x = continent, y = percentage_lifeExp_change)) +
	geom_bar(stat = "identity", aes(fill = continent))

#stack_barchart
gapminder %>%
	group_by(year, continent) %>% 
	summarize(mean_lifeExp = mean(lifeExp)) %>% 
	arrange(continent, year) %>% 
	group_by(continent) %>% 
	mutate(mean_lifeExp_delta = mean_lifeExp - lag(mean_lifeExp),
				 percentage_lifeExp_change = (mean_lifeExp_delta/mean_lifeExp)*100) %>% 
	filter(year>1952) %>% 
	# filter(year == Y) %>%
	ggplot(aes(x = continent, y = percentage_lifeExp_change)) +
	geom_bar(stat = "identity", aes(fill = year))



plot_data <- gapminder %>%
	group_by(year, continent) %>% 
	summarize(mean_lifeExp = mean(lifeExp)) %>% 
	arrange(continent, year) %>% 
	group_by(continent) %>% 
	mutate(mean_lifeExp_delta = mean_lifeExp - lag(mean_lifeExp),
				 percentage_lifeExp_change = (mean_lifeExp_delta/mean_lifeExp)*100) %>% 
	filter(year>1952)

plot_data %>% 
	group_by(continent) %>% 
	summarize(sum(percentage_lifeExp_change))
