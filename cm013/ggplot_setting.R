rm(list = ls())
library(gapminder)
library(tidyverse)
library(forcats)

jdat <- gapminder %>% 
	filter(continent != "Oceania") %>% 
		mutate(country2 = fct_reorder(country, -1*pop)) %>% 
	arrange(year, country)
j_year <- 2007

q <- jdat %>% 
	filter(year == j_year) %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp))

q + geom_point() + scale_x_log10(limits = c(230, 63000))
q + geom_point(pch =21, size = 8, fill = I("darkorchid1")) + 
	scale_x_log10(limits = c(230, 63000))

# Now, I want to change the size of each ball regarding population
q + geom_point(aes(size = pop), pch =21) + scale_x_log10(limits = c(230, 63000))

# But I want to suppress the legend in figure
q + geom_point(aes(size = pop), pch =21, show.legend = FALSE) + 
	scale_x_log10(limits = c(230, 63000))

# I can also have control on the size range using scale_size_continuous
q + geom_point(aes(size = pop), pch =21, show.legend = FALSE) +
	scale_x_log10(limits = c(230, 63000)) + scale_size_continuous(range = c(1,20))

# Now, I want to map colour to a factor vector
(r <- q + geom_point(pch =21, aes(size = pop), show.legend = FALSE) +
	scale_x_log10() + facet_wrap(~ continent) + ylim(c(39, 87)) + 
	scale_size_continuous(range = c(1,40)))
r + aes(fill = continent)

str(country_colors)
head(country_colors)

# Order of country colours is not on alphabetical case, it is based on population of country in
# each continent.

# "scale" in ggplot2 controls the mapping from variable to an aesthetic
# We may have control on aesthatic parameters mapped to each varaiable using "scale"
# Sofar, colouring scheme was determined automatically by ggplot2
# Now, I want to change colouring scheme masnually.

# "scale_fill_manual()" is for customization of discrete scales
# The main arguement is "vallue" by which the colour vector being assigned to a variable is defined
# It is preferrable to assign names to the vector of colours.
# By assigning names we are not worried of changes in factor levels or row order since ggplot is
# cognizant of assigned names when assigning colour to factor.

r + aes(fill = country) + scale_fill_manual(values = country_colors)

# The whole thing:
j_year <- 2007
jdat %>% 
	filter(year == j_year) %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp)) +
	geom_point(pch = 21, show.legend = FALSE) + scale_x_log10(limits = c(230,63000)) + ylim(39, 89) +
	aes(size = pop, fill = country) + 
	scale_size_continuous(range = c(1,40)) + 
	scale_fill_manual(values = country_colors) +
	facet_wrap(~continent)

# Here, we see that ggplot distinguishes vector names
j_colours <- c("#00ffff", "#ff00ff", "#ff0000", "#0000ff")
names(j_colours) <- c("Germany", "Turkey", "Iran","India")
jdat %>% 
	filter(country %in% c("Iran", "Germany", "Turkey", "India")) %>% 
	droplevels() %>% 
	ggplot(aes(x = year, y = lifeExp)) +
	geom_line(aes(colour = fct_reorder2(country, year, lifeExp))) +
	labs(colour = "country") + scale_colour_manual(values = j_colours)

