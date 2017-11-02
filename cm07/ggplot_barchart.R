rm(list = ls())
library(gapminder)
library(tidyverse)
# geom_bar() counts the number of observations by default

gapminder %>% 
	ggplot(aes(x = continent)) + geom_bar()

gapminder %>% 
	group_by(continent) %>% 
	tally() %>% 
	ggplot(aes(x = continent, y = (n/sum(n))*100)) +
	geom_bar(stat = "identity")

gapminder %>%
	ggplot(aes(x = continent)) +
	geom_bar()


reorder(gapminder$continent, gapminder$continent, length)

gapminder %>% 
	ggplot(aes(x = reorder(continent, continent, length))) + 
	geom_bar()
library(ggthemes)

# theme_calc() is from ggthemes library
gapminder %>% 
	group_by(continent) %>% 
	summarize(n_country = n_distinct(country)) %>% 
	mutate(continent = reorder(continent, desc(n_country))) %>% 
	ggplot(aes(x = continent, y = n_country)) +
	geom_bar(stat = "identity", width = 0.2) +
	theme_calc()
