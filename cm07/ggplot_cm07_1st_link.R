rm(list = ls())
library(tidyverse)
library(gapminder)
gapminder %>% 
	plot(aes(x = gdpPercap, y = lifeExp, colour = continent)) +
	geom_point() +
	scale_x_log10()


p <- gapminder %>%
	ggplot(aes(x = gdpPercap, y = lifeExp))
p + aes(colour = continent) + scale_x_log10() +
	geom_point() + geom_smooth(lwd = 3, se = FALSE)


p + geom_point(aes(alpha = 1/3, size = 3)) +
	facet_wrap(~ continent) +
	geom_smooth(lwd = 2, se = FALSE)


gapminder %>%
	ggplot(aes(x = year , y = lifeExp, colour = continent)) +
	geom_jitter(aes(alpha = 1/3, size = 3)) +
	geom_smooth(lwd =2, se = FALSE) +
	scale_colour_manual(values = continent_colors) + 
	facet_wrap( ~ continent, scales = "free_x")

gapminder %>%
	ggplot(aes(x = year, y = lifeExp,group = country, colour = country))+
	geom_line(lwd = 1, show_guide = FALSE) +
	facet_wrap( ~ continent , scales = "free_x")+
	scale_colour_manual(values = country_colors)+
	# scale_colour_brewer()+
	theme_bw() +
	theme(strip.text = element_text(size = rel(1.1)))


gapminder %>%
	filter(country == "Iran") %>% 
	ggplot(aes(x = year, y = lifeExp))+
	geom_line()


y <- gapminder %>% 
	ggplot(aes(x = year, y = lifeExp))+
	geom_point()

y + geom_smooth(lwd = 2, se = FALSE)+
	geom_smooth(lwd = 2, se = FALSE, method = "lm", colour = "orange") 

# Using "group= " vs not using it
y + facet_wrap(~ continent)+
	geom_line(aes(group = country))

y + geom_smooth(lwd = 2 , se = FALSE) +
	geom_smooth(aes(group = continent, colour = continent),
							lwd = 2, se = FALSE)


gapminder %>% 
	filter(country == "Zimbabwe") %>% 
	ggplot(aes(x = year, y = lifeExp)) +
	geom_line() + geom_point()

jcountries <- c("Canada", "Rwanda", "Cambodia", "Mexico")

gapminder %>%
	filter(country %in% jcountries) %>% 
	ggplot(aes(x = year, y = lifeExp, colour = country)) +
	geom_line(aes(group = country)) + geom_point()

gapminder %>%
	filter(country %in% jcountries) %>% 
	ggplot(aes(x = year, y = lifeExp, colour = reorder(country, -1*lifeExp, max))) +
	geom_line(aes(group = country)) + geom_point()


gapminder %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp)) +
	scale_x_log10() +
	geom_bin2d()


sessionInfo()
