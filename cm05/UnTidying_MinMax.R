rm(list = ls())
library(gapminder)
library(tidyverse)
tmp_lifeExp <- gapminder %>% 
	group_by(year, continent) %>% 
	filter(min_rank(desc(lifeExp))<2 | min_rank(lifeExp)<2) %>% 
	arrange(year,continent, lifeExp) %>% 
	# filter(year>2000) %>% 
	select(-pop, -gdpPercap) %>%
	ungroup() %>% 
	mutate(year = as.factor(year),
		MinMax = rep(c("min", "max"), nlevels(continent)*nlevels(year)),
		MinMax = as.factor(MinMax)) %>% 
	select(-country) %>% 
	spread(key = MinMax, value = lifeExp) %>% 
	rename(min_lifeExp = min , max_lifeExp = max)

tmp_country <- gapminder %>% 
	group_by(year, continent) %>% 
	filter(min_rank(desc(lifeExp))<2 | min_rank(lifeExp)<2) %>% 
	arrange(year,continent, lifeExp) %>% 
	# filter(year>2000) %>% 
	select(-pop, -gdpPercap) %>%
	ungroup() %>% 
	mutate(year = as.factor(year),
		MinMax = rep(c("min", "max"), nlevels(continent)*nlevels(year)),
				 MinMax = as.factor(MinMax)) %>% 
	select(-lifeExp) %>% 
	spread(key = MinMax, value= country) %>% 
	rename(min_country = min , max_country = max)


inner_join(tmp_country, tmp_lifeExp) %>% 
	arrange(year) %>% 
	print(n = Inf)



gapminder %>% 
	group_by(year, continent) %>% 
	filter(min_rank(desc(lifeExp))<2 | min_rank(lifeExp)<2) %>% 
	arrange(year,continent, lifeExp) %>% 
	filter(year>2000) %>% 
	select(-pop, -gdpPercap) %>%
	ungroup() %>% 
	mutate(year = as.factor(year)) %>% 
	spread(key = year, value = lifeExp)
	