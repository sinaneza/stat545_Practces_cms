rm(list = ls())
library(gapminder)
#Tidyverse includes many packages
#Tidyverse embraces a special type of date frame named "tibble"
# The core package of "tidyverse" is "dplyr"
# "plyr" functions focus on different flavours of input (e.g. 'arrays', 'data frames', and 'lists')
#However, "dplyr" is just focused on 'dataframe' type of an input
library(tidyverse)

gapminder %>% 
	filter(country == "Canada")

lm(lifeExp ~ log10(gdpPercap), data = filter(gapminder, country == "Canada"))


#filter():
filter(gapminder, lifeExp < 29)
filter(gapminder,continent == "Asia", lifeExp >= 70)
filter(gapminder, country == "Iran", lifeExp>=70)
filter(gapminder, country == "Rwanda", year >= 1979)

Canada <- filter(gapminder, country == "Canada")
Canada$country == c("Canada", "Iran")
Canada$country %in% c("Canada", "Iran")

gapminder %>% 
	head

gapminder %>%
	head(4)

gapminder %>% 
	select(country, year) %>% 
	head(4)

gapminder %>% 
	filter(country == "Cambodia") %>% 
	select(-pop)
	# select(country, year)


gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")]

gapminder$country == "Cambodia"


iris
structure(iris)
names(iris)
#both filter() and select() work on non-tibblized datasets
filter(iris, Sepal.Width > 4)
select(iris, Sepal.Length, Sepal.Width)


#dplyr functions for a single dataset

my_gap <- gapminder

my_precious <- my_gap %>% 
	filter(country == "Canada")

#mutate(): adds a new variable to our data set
# For instance, we want to recover the information related to gdp from gdpPercap
mutate(gapminder, gdp = gdpPercap * pop)

my_gap %>% 
	mutate(gdp = gdpPercap * pop)

nlevels(gapminder$country)
levels(gapminder$country)
#rep(): used to repeat the vector
c(1,2) %>% 
	rep(2)

c(1,2) %>% 
	rep(3)

#Now I want to find the ratio of gdpPecap for each country, relative to Canada.
#I need to make sure that values which are divided are for the same year.

ctib <- filter(my_gap, country == "Canada")
my_gap <- my_gap %>% 
	mutate(tmp = rep(ctib$gdpPercap, nlevels(country)),
				 gdpPercapRel = gdpPercap/tmp,
				 tmp = NULL)
# It is possible to get rid of the varibales by 'mutate()', using
# 'mutate(tmp = , tmp = NULL)'

#Draw the "Frequency Plot" for "gdpPercaprel"
#To see the fact that gdp per capita is well below its value for Canada
my_gap %>% 
	filter(country == "Canada")
my_gap %>%
	ggplot(aes(gdpPercapRel)) +
	geom_freqpoly()

summary(my_gap$gdpPercapRel)
hist(my_gap$gdpPercapRel)

my_gap %>% 
	filter(gdpPercapRel>1,
				 year == 2007)

# Arrange(): To reorder the rows in a data frame by one of the variables
# If "arrange()" has two arguments, then the first one is in priority
# meaning that it first arrange the dataset rrelated to firest argument, 
# then, if two rows were the same according to the first argument, the would be arranged
# per the second argument

my_gap %>% 
	arrange(year)

my_gap %>% 
	arrange(year, country)

my_gap %>% 
	arrange(year, continent)

#Two WAYS TO ARRANGE IN DESCENDING ORDERS
#1)
my_gap %>% 
	arrange(-gdpPercap, -pop)
#2)
my_gap %>% 
	arrange(desc(gdpPercap))


my_gap %>% 
	filter(year ==2007) %>% 
	arrange(lifeExp)

my_gap %>% 
	filter(year == 2007) %>% 
	arrange(desc(lifeExp))

#Renaming the variables in two ways:
#1) rename() 2) select()
my_gap %>% 
	rename(gdp_per_cap = gdpPercap,
				 gdp_per_cap_rel = gdpPercapRel)


my_gap %>% 
	select(gdp_per_cap = gdpPercap,
				 yr = year,
				 country, life_exp = lifeExp)

#everything(): keeps all any other variables remained
my_gap %>% 
	select(gdp_per_cap = gdpPercap,
				 everything())

# 'group_by()': adds extra structure to your dataset – grouping information – 
## which lays the groundwork for computations within the groups.
## 'summarize()': takes a dataset with "n" observations, computes requested summaries,
## and returns a dataset with 1 observation.
# Window functions: take a dataset with n observations and return a dataset with n observations.


#Counting things by "group_by()"
my_gap %>% 
	group_by(continent) %>% 
	summarize(n = n())
#n(): Counts number of observations in the current group
#n() can only be used from within 'summarise()', 'mutate()' and 'filter()'
#It can be seen that 'nrow()' doesnt't work inside 'summarize()'
my_gap %>% 
	group_by(continent) %>% 
	summarize(n = nrow())


# getting result somehow similar to previous case
table(gapminder$continent)
str(table(gapminder$continent))
# we can see the disadvantage in table()
# table() makes a dataset in which continent are not factors any more, they are characters
# we will lose any defined order for factors in this case

# tally(): counts number of rows in each group (doesn't need 'summarize()')
# tally() is just dedicated to grouped datasets
# tally() shouldnt be used inside summarize()
my_gap %>% 
	group_by(continent) %>% 
	tally()

#'Count()': is a function grouping the data and computing the number of observations in each
##group simultaneously
# Thus, 'count()' is applicable to ungrouped data set and there is no need for 
# to be grouped
count(gapminder, continent)

my_gap %>% 
	group_by(continent) %>% 
	count(country)

#uniqe(x): returns a vector, data frame or array like x
#but with duplicate elements/rows removed.


my_gap %>% 
	filter(continent == "Asia") %>%
	select(continent, country) %>% 
	unique() %>%
	nrow()

# n_distinct(): Does sth similar to 'length(unique(x))'.
# It efficiently counts the number of unique values in a set of vector
my_gap %>% 
	filter(continent == "Asia") %>%
	select(country) %>% 
	n_distinct()

my_gap %>% 
	filter(continent == "Asia") %>%
	n_distinct(country)
# This didnt work since n_distinct has a single input which is a vector

Asia <- my_gap %>% 
	filter(continent == "Asia")
n_distinct(Asia$country)
#Voila! This worked

#Now I want to group the data to find number of countries in each continent
my_gap %>% 
	group_by(continent) %>%
	summarize(n_country = n_distinct(country))
#Another way to get the same result using 'length(unique(x))'
my_gap %>% 
	group_by(continent) %>% 
	summarize(length(unique(country)))


#function used in 'summarize()' are classical statistic summaries, such as 'mean()',
#'min()', 'max()', 'IQR()', 'var()', 'sd()', and 'mad()', getting n input
#and giving a single output.
#IQR(): Finds the difference b/w 1st and 3rd quantile.
#mad(x): computes median absolute deviation, i.e., 
# the median of the absolute deviations from the median
#sd(): standard deviation
#var(): variance

my_gap %>% 
	group_by(continent) %>% 
	summarize(life_exp_mean = mean(lifeExp))

#summarize_each(): applies the same summary function(s) to multiple variables.
my_gap %>% 
	group_by(continent) %>% 
	summarize_each(funs(mean), lifeExp, gdpPercap)


my_gap %>% 
	group_by(continent) %>% 
	summarize_each(funs(mean, median), lifeExp, gdpPercap)

my_gap %>% 
	group_by(year) %>% 
	summarize(lifeExp_mean = mean(lifeExp),
						lifeExp_median = median(lifeExp))
#grouped mutate:
# We want to find the life epectancy that each country has gained since 1952
my_gap %>% 
	group_by(country) %>% 
	mutate(lifeExp_gain = lifeExp - first(lifeExp))

my_gap %>% 
group_by(continent) %>% 
	summarize(lifeExp_min = min(lifeExp), lifeExp_max = max(lifeExp))


#rank(): Shows the standing(rank) of each value if it was ordered increasingly (minimum first)
#In the case of any ties in ranking, 'ties.method' argument defines how the function will deal with tie 
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged
(r3 <- rank(x2, ties.method= "first")) # first occurrence wins
(r4 <- rank(x2, ties.method= "last"))   #  last occurrence wins
rank(x2, ties.method= "random") # ties broken at random
(rma <- rank(x2, ties.method= "max"))  # as used classically
(rmi <- rank(x2, ties.method= "min"))  # as in Sports

#Now, I want to find the countries having best and worst life expectancy in different years
# I want to have the name of countries in my dataset as well
# 'rank()' is helpful in finding the best and worst countries in life exoectancy point of view
#print(n= Inf): Helps us in watching the whole data set in our screnn
my_gap %>% 
	group_by(year) %>% 
	filter(min_rank(lifeExp) < 2 | min_rank(desc(lifeExp)) < 2) %>% 
	arrange(year) %>%
	select(continent, country, year, lifeExp) %>% 
	print(n = Inf)

# Now, lets move forward. Lets find countries with maximum and minimum life expectancy 
# in all continents in different years
my_gap %>% 
group_by(continent, year) %>% 
	filter(min_rank(lifeExp)<2 | min_rank(desc(lifeExp)) < 2) %>% 
	arrange(continent, year) %>%
	select(continent, country, year, lifeExp) %>% 
	print(n = Inf)

# In the previous code we can filter the data to each continent to limit our dataset
# An uninteresting way of doing so is filtering the data to each continent at very first
# stages.

my_gap %>% 
	filter(continent == "Asia") %>% 
	group_by(year) %>%
	filter(min_rank(desc(lifeExp)) <2 | min_rank(lifeExp) < 2) %>% 
	select(continent, country, year, lifeExp) %>%
	arrange(year) %>% 
	print(n = Inf)

asia <- my_gap %>% 
	filter(continent == "Asia") %>% 
	group_by(year)

asia %>% 
	mutate(le_rank = min_rank(lifeExp),
				 le_rank_desc = min_rank(desc(lifeExp))) %>% 
	filter(country %in% c("Afghanistan", "Iran", "Thailand", "Japan")) %>% 
	print(n = Inf)

# If just minimum or maximum was desired, we could use the function 'top_n()'
# 'top_n(x, n)': n=1; just the first roew having maximum
# n=2; just the first two rows having maximum
# n= -1; just the first row having the minimum value.
# The first input of 'top_n()' should be a data frame
top_n(data_frame(gapminder$lifeExp), 2)
top_n(gapminder, 2, wt = lifeExp)

# To get the first maximums
my_gap %>% 
	filter(continent == "Asia") %>% 
	group_by(year) %>% 
	top_n(5, wt = lifeExp) %>% 
	arrange(year) %>% 
	print(n = Inf)

# Two ways to get the first minimums
#1)
my_gap %>% 
	filter(continent == "Asia") %>% 
	group_by(year) %>% 
	top_n(-5, wt = lifeExp)
#2)
my_gap %>% 
	filter(continent == "Asia") %>% 
	group_by(year) %>% 
	top_n(5, wt = desc(lifeExp))


#lead(): Is useful when we want to compare a value in a variable with its coming values
# lag(): Is useful when we want to compare a value in a variable with its previous values

cbind(lead = lead(my_gap$lifeExp),
			lifeExp = my_gap$lifeExp,
			lag = lag(my_gap$lifeExp))

# Now, I want to find which country encountered with sharpest decrease in life expectancy
#  within 5 years in each continent
# top_n(): applies to both grouped and un-grouped dataset
# If it is grouped dataset it computes its output fot each group
# If it is not grouped, the noutput is just computed for the whole dataset
my_gap %>% 
	group_by(continent, country) %>% 
	mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
	summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
	print(n = Inf) %>% 
	top_n(-1, wt = worst_le_delta)

# But I want to keep years of occurance!!
my_gap %>% 
	group_by (country) %>% 
	mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
	top_n(-1, wt = le_delta) %>% 
	group_by(continent) %>% 
	top_n(-1, wt = le_delta) %>% 
	print(n =Inf)


my_gap %>% 
	group_by(country) %>% 
	ungroup()


my_gap %>% 
	group_by(continent, country) %>% 
	mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
	summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
	summarize(min_le_del = min(worst_le_delta))

# ungroup() cancels all created groups
my_gap %>% 
	group_by (continent, country) %>% 
	mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
	top_n(-1, wt = le_delta) %>% 
	ungroup() %>% 
	top_n(-1, wt = le_delta) %>% 
	print(n =Inf)