# Keep stuff in data frames
# Keep your data frames tidy; be willing to reshape your data often
# Use factors and be the boss of them
rm(list = ls())
library(gapminder)
library(tidyverse)

Year <- gapminder$year
life_exp <- gapminder$lifeExp
ggplot(aes(x = year, y = gdpPercap))
# There is an error since ggplot just accepts data frames
# Most R potting objects such as 'lattice' and 'plot' use dataframes

# We can pass the dataframe to ggplot and use variables previously defined as new arguments.
ggplot(data = gapminder, aes(x = Year, y = life_exp)) + geom_jitter()

# tibble(): A better way to construct a data frame
# Advantage of tibble compared to data.farame is that it keeps characters and doesn't convert
# them to factors.

# tribble(): is similar to 'tibble()', but data is imported similar to a table

my_dat1 <- tibble(x = 1:5, y = x^2, text = c("alpha", "beta", "gamma", "delta", "epsilon"))
my_dat2 <- tribble(
	~x,   ~y,    ~text,
	1,     1,    "alpha",
	2,      4,   "beta"
)

str(my_dat1)
str(my_dat2)

my_dat1 %>% 
	ggplot(aes(x = x, y = y)) +
	geom_line() + geom_text(aes(label = text))
# Some functions such as 'cor()', calculating correlation between tw variables, don't accept 
# data as their initial input.
# To do that and prevent multiple typing, it is better to use 'with()'
# 'with(data, function)' imports data as initial input to function, so the function is applied 
# in the data frame span
?cor
cor(x = year, y = lifeExp, data = gapminder)
cor(x = gapminder$year, y = gapminder$lifeExp)
cor(x = gapminder$lifeExp, y = log10(gapminder$gdpPercap))

with(gapminder,cor(x = lifeExp, y = log10(gdpPercap)))

# An alternative to 'with()' is '%$%' in 'magrittr' package.
library(magrittr)
gapminder %$%
	cor(lifeExp, log10(gdpPercap))
