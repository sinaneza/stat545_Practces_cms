rm(list = ls ())
library(tidyverse)
library(gapminder)

#
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram(binwidth = 1)

#The problem that default 'position = stack' has
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram(aes(fill = continent))

# The problem of stacked histograms solved by 'position = identity'
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram(aes(fill = continent), position = "identity")

gapminder %>% 
	filter(continent == "Europe") %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram()

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram(aes(fill = continent), position = "identity") +
	facet_wrap(~ continent)


# Frequency Polygons to display counts in line:

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_freqpoly()

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	aes(colour = continent) +
	geom_freqpoly()


# geom_density() to get smoother plots than 'geom_freqpoly()'
gapminder %>% 
	ggplot(aes(x = lifeExp)) +

# The value defined by adjust defines the prtion of default band width in new band width
# (new band width = adjust * defualt bandwidth)
# Band width is sth similar to bin in 'geom_histogram()' and 'geom_freqpoly()'
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_density(adjust = 1)

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_density(adjust = 0.2)

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	aes(colour = continent) +
	geom_density()

# Showing shaded swept area by 'fill':
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	aes(fill = continent) +
	geom_density()

# 'alpha' has application here as well
gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	aes(fill = continent) +
	geom_density(alpha = 0.2)

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_density() +
	facet_wrap(~continent)

gapminder %>% 
	ggplot(aes(x = lifeExp)) +
	geom_histogram(aes(fill = continent)) +
	facet_grid(continent ~., scales = "free_x")

# If boxplot is desired, it is independant variable should be described either as factor 
# or as group
gapminder %>%
	ggplot(aes(x = year, y = lifeExp)) +
	geom_boxplot(aes(group = year))

gapminder %>% 
	mutate(year = as.factor(year)) %>% 
	ggplot(aes(x = year,y= lifeExp)) +
	geom_boxplot()

gapminder %>% 
	mutate(year = as.factor(year)) %>% 
	ggplot(aes(x = year,y= lifeExp)) +
	geom_boxplot() + facet_wrap(~continent, scales = "free_x")

# geom_violin(): Displays the distribution in a more compact form. It is 
# a mirrored density plot
gapminder %>%
	ggplot(aes(x = year,y= lifeExp)) +
	geom_violin(aes(group = year)) +
	geom_jitter(alpha = 1/3, position = position_jitter(width = 0.2, height = 0)) +
	geom_boxplot(alpha = 1/3, aes(group = year))

gapminder %>%
	ggplot(aes(x = year,y= lifeExp)) +
	geom_violin(aes(group = year)) +
	geom_jitter(alpha = 1/4, position = position_jitter(width = 0.2, height = 0)) +
	geom_boxplot(alpha = 1/3, aes(group = year)) +
	geom_smooth(se = FALSE)
