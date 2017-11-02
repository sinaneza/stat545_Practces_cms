rm(list = ls())
library(gapminder)
library(tidyverse)

jcountries <- c("Canada", "Germany", "Turkey", "Iran", "Cambodia")

x <- gapminder %>% 
	filter(country %in% jcountries) %>% 
	droplevels() %>% 
	mutate(country = reorder(country, lifeExp, mean))

ggplot(x, aes(x = year, y = lifeExp, colour = country)) +
	geom_point() +
	geom_line()

library(RColorBrewer)
display.brewer.all()
# 1. Sequential palettes are suited to ordered data that progress from low to high.
# Lightness steps dominate the look of these schemes, with light colors for low data values to 
# dark colors for high data values.
# 
# 2. Diverging palettes put equal emphasis on mid-range critical values and extremes at both 
# ends of the data range. The critical class or break in the middle of the legend is emphasized
# with light colors and low and high extremes are emphasized with dark colors that have 
# contrasting hues. 
# 
# 3. Qualitative palettes do not imply magnitude differences between legend classes, and hues
# are used to create the primary visual differences between classes.
# Qualitative schemes are best suited to representing nominal or categorical data.

display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
display.brewer.all(type = "div")

jcolours <- brewer.pal(n=8, "Dark2")[seq_len(nlevels(x$country))]
names(jcolours) <- levels(x$country)

ggplot(x, aes(x = year, y = lifeExp, colour = country)) +
	geom_point() +
	geom_line() +
	scale_colour_manual(values = jcolours)


