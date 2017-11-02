# Be the boss fo your factors

# In base R functionds such as 'read.table()' and 'data.frame()' there is an urge to convert 
# "characters" to "factors".

# To suppress this urge, we should use 'stringsAsFactors = FALSE' in 'read.table()' and 'data.frame()'
# or even better using 'tidyverse':: 'tibble()' (to create a data frame), 'read_csv()', 'read_tsv()'

# "focats" is a non-core package in tidyverse, meaning that it is installed via 'install.packages()';
# however, it is not ;loaded via "library(tidyverse)". It should be loaded separately via 
# "library(forcats)"
# Functions in forcats generally start by fct_.
# There are not grnerally similar counterparts in R base functions
rm(list = ls())
library(gapminder)
library(tidyverse)
library(forcats)

gapminder
str(gapminder)
gapminder$continent
str(gapminder$continent)
levels(gapminder$continent)
nlevels(gapminder$continent)
class(gapminder$continent)


# 'fct_count()' is a function in forcats equivalent to 'count()'
# 'fct_count()'counts the number of observations for each factor level

gapminder %>%
	count(country)

gapminder %>% 
	fct_count(country) #It didnt work since it is a single input function

fct_count(gapminder$country)

# Drop Unused Levels
# When the datframe is filtered to specific levels of a factor, the levels eliminated from the dataframe
# are not dropped from factor levels
# To do that we have two alternatives:
# 1) droplevels(): This function applies to the whole dataframe and drops the levels not existing
#new dataset
# 2) fct_drp(): This is appied to a vector comprised of factors

h_country <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>% 
	filter(country %in% h_country)

nlevels(h_gap$country)

h_gap_drop <- h_gap %>% 
	droplevels()

nlevels(h_gap_drop$country)
nlevels(h_gap_drop$continent)

fct_drop(h_gap$country) %>% 
	nlevels()

# Reordering Factors:
# Three purposes of factor reordering
# 1) By frequency of each factor level ('fct_infreq')
# 2) By another varible( a function like mean applied to that variable) ('fct_reorder')
# 3) Changing the order arbitrarily on your own basis ('fct_relevel')

gapminder$continent %>% 
	levels()

# 1) Order by their frequency
gapminder$continent %>% 
	fct_infreq() %>% 
	levels()

# 2) Order by another vraible and a function applied to it
gapminder$country %>% 
	levels()

fct_reorder(gapminder$country, gapminder$lifeExp) # function is set to median by default
fct_reorder(gapminder$country, gapminder$lifeExp, .desc= TRUE) # Reordering in a descending order
fct_reorder(gapminder$country, gapminder$lifeExp, min)
fct_reorder(gapminder$country, gapminder$lifeExp, max)

# Its application in graphs:
gap_asia_2007 <- gapminder %>%
	filter(year == 2007, continent == "Asia")

gap_asia_2007 %>% 
	ggplot(aes(x = lifeExp, y = country)) +
	geom_point()

gap_asia_2007 %>%
	ggplot(aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
	geom_point()

# 'fct_reorder2' reorders by applying a function having two inputs
# It is mainly useful when we want to reorder the legend regarding the lines drawn
# (legends in the same order of drawn lines)

h_country <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>% 
	filter(country %in% h_country) %>% 
	droplevels()

h_gap %>% 
	ggplot(aes(x = year, y = lifeExp, colour = country)) + geom_line()

# This orders the legends according to lifeExp in the end point of graph (2007)
# The default function in 'fct_reorder2' is 'last2()'
# 'last2' considers the order of countries regarding the last year
h_gap %>%
	ggplot(aes(x = year, y = lifeExp,
						 colour = fct_reorder2(country, year, lifeExp))) +
	geom_line()

# It is an alternative to previous one
# This orders the legends based on median of lifeExp
h_gap %>%
	ggplot(aes(x = year, y = lifeExp,
						 colour = fct_reorder(country, -lifeExp))) +
	geom_line()


# 'fct_rev()' reverses the order
country_reorder <- fct_reorder(gapminder$country, gapminder$lifeExp)
head(country_reorder)

country_rev <- fct_rev(country_reorder)
head(country_rev)

# 3) Change the levels arbitrarily
h_gap$country %>%
	levels()
# 'fct_relevel' changes the levels of factors on your selection
# 'fct_relevel' puts levels mentioned in it at first

h_gap$country %>% 
	fct_relevel("Romania", "Haiti") %>% 
	levels()

i_gap <- gapminder %>% 
	filter(country %in% c("United States", "Sweden", "Australia")) %>% 
	droplevels()

i_gap$country %>% 
	levels()

# 'fct_recode()' to change some factor levels
i_gap$country %>% 
	fct_recode("USA" =  "United States", "Oz" = "Australia") %>% 
	levels()

# Introduced Functions:
# 1) fct_count
# 2) fct_drop
# 3) fct_infreq
# 4) fct_reorder
# 5)fct_reorder2
# 6) fct_rev: Reversiong the levels
# 7) fct_relevel: Put the mentioned levels at first
# 8) fct_recode: Change the name of levels

# Considerable Point: If wanting to order the legend, reordering should be applied to colour section
# of ggplot