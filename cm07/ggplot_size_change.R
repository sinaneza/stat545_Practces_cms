rm(list = ls())
library(gapminder)
library(tidyverse)

jYear <- 2007 # this can obviously be changed
jPch <- 21
jDarkGray <- 'grey20'
jXlim <- c(150, 115000)
jYlim <- c(16, 100)


gapminder %>% 
	filter(year == jYear) %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp)) +
	scale_x_log10(limit = jXlim) + ylim(jYlim) +
	geom_point(aes(fill = country,
								 size = sqrt(pop/pi)),
						 colour = jDarkGray,
						 pch = jPch,
						 show_guide = FALSE) +
	scale_size_continuous(range = c(1,40)) +
	scale_fill_manual(values = country_colors) +
	facet_wrap( ~ continent, scales = "free_x") +
	coord_fixed(1/43) + theme_bw() + theme(strip.text = element_text(size = rel(1.1)))
	
