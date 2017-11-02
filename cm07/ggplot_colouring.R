rm(list = ls())
library(tidyverse)
library(gapminder)
my_gap <- gapminder

# To globally apply colour
p <- my_gap %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent))



p + scale_x_log10() + geom_point()

# To apply colour just on points
p2 <- my_gap %>% 
	ggplot(aes(x = gdpPercap, y = lifeExp))

p2 + scale_x_log10() + geom_point(aes(colour = continent))
