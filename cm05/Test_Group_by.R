rm(list = ls())
library(gapminder)
library(tidyverse)
my_gap <- gapminder

my_gap %>%
	select(continent, country) %>% 
	unique() %>% 
	group_by(continent) %>% 
	summarize(n_countries = length(country))


my_gap %>% 
	group_by(continent) %>% 
	summarize(n_countries = n_distinct(country))


contin_lifeExp_MinMax <- my_gap %>% 
	group_by(year, continent) %>%
	filter(min_rank(lifeExp) < 2 | min_rank(desc(lifeExp)) < 2) %>% 
	arrange(year, lifeExp)

contin_lifeExp_MinMax %>% 
	filter(continent == "Europe") %>% 
print(n = Inf)

my_gap %>% 
	group_by(year, continent) %>% 
	filter(min_rank(gdpPercap) < 3 | min_rank(desc(gdpPercap)) < 3) %>% 
	arrange(year) %>% 
	filter(continent == "Europe") %>% 
	# arrange(year, gdpPercap) %>% 
	print(n = Inf)

my_gap %>%
	select(country, year, continent, lifeExp) %>%
	group_by(continent, country) %>%
	## within country, take (lifeExp in year i) - (lifeExp in year i - 1)
	## positive means lifeExp went up, negative means it went down
	mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
	## within country, retain the worst lifeExp change = smallest or most negative
	summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
	## within continent, retain the row with the lowest worst_le_delta
	top_n(-1, wt = worst_le_delta) %>% 
	arrange(worst_le_delta)
