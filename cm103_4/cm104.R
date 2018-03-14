rm(list = ls())
library(purrr)
library(tidyverse)
# repurrrsive is a package containing lists for further train
library(repurrrsive)
str(got_chars)
str(got_chars, max.level = 1)
str(got_chars, list.len =6)

library(listviewer)
jsonedit(got_chars, mode = "view")
str(got_chars[[1]])
got_chars[[1]][["aliases"]]

# Now I want to find aliases for all lists included, solution: 'purrr: map()'
(aliases <- map(got_chars, "aliases"))

# set_names(): To define names for elements of any object (vector, dataframe)
set_names(got_chars,
					map(got_chars, "name"))
(aliases <- set_names(aliases, map_chr(got_chars, "name")))

# To subset lists with specified names:
#1st way:
aliases[c("Theon Greyjoy", "Asha Greyjoy","Brienne of Tarth")]

#2nd way
(indice <- which(names(aliases) %in% c("Theon Greyjoy", "Asha Greyjoy","Brienne of Tarth")))
aliases[indice]			#2nd way

# two ways to collapse a char vector: paste(,collase = ), stringr::str_c(,collapse = )
paste(aliases[[1]], collapse = "|")
library(stringr)
str_c(aliases[[1]], collapse = "|")

# three ways to apply a newly defined function to a list
# 1)defining function outside of the map
# 2)defining function inside the map
# 3)using '~'

# 1)
collapse1 <- function(x) paste(x, collapse = " | ")
collapse2 <- function(x) str_c(x, collapse = " | ")
collapse1(aliases[[1]])
collapse2(aliases[[1]])
map(aliases, collapse1)
jsonedit(map(aliases, collapse1), mode = "view")
jsonedit(aliases, mode = "view")
map(aliases, collapse2)

# 2)
map(aliases, function(x) paste(x, collapse = " | "))
map(aliases, function(x) str_c(x, collapse = " | "))

# 3)
map(aliases, ~paste(.x, collapse = " | "))
map(aliases, ~str_c(.x, collapse = " | "))

map(aliases, paste, collapse = " | ")
map(aliases, str_c, collapse = " | ")

# 'tibble::enframe()': to convert a list to a data frame; 
# each element of a list defines a row
# The enframed elements of the list remain as a list inside a dataframe 
# Something analagous to nested data frame

# 'purrr::map_df()': converts elements of a list to a data frame; 
# each element of a list define a column;
# The elements are converted to atomic vectors, compared to 'tibble::enframe'
aliases %>% 
	map(length) #Elements have different lengths; conversion to a data frame is not possible

(Enframe <- map(aliases, ~str_c(.x, collapse = "|")) %>% 
	tibble::enframe())

str(Enframe)

Enframe %>% 
	View()

map_df(aliases, ~ str_c(.x, collapse = " | "))
(MapDf <- map_df(aliases, ~ paste(.x, collapse = " | ")))
str(MapDf)

View(MapDf)

(nms <- map(got_chars, "name"))
(birth <- map(got_chars, "born"))
map2(nms, birth,
		 function(x,y) paste(x, "was born", y))

map2_chr(nms, birth,
					function(x,y) paste(x, "was born", y))

## Nesting:
# Sometimes it is required to extract two variables in agrouped data frame
# Nesting makes this possible for us
# By nesting we make a variable composed of lists. Each list contains data related to each group.
# Advantage: wew can apply computation on each list using 'map()'.
library(gapminder)
gapminder %>%
	group_by(continent, country) %>% 
	summarise(range = range(lifeExp))
#We can apply summarise_each, but this is not applicable for all calculations
gapminder %>%
	group_by(continent, country) %>% 
	summarize_each(funs(min, max), lifeExp)

(gap_nested <- gapminder %>% 
	group_by(continent, country) %>% 
	nest())
## To apply country names as nemes of elements in "data"
# 1)
(gapnested2 <- gap_nested %>% 
	with(set_names(data, country)))

# 2)
gap_nested3 <- (set_names(gap_nested[["data"]], gap_nested[["country"]]))

# for the first observation:
str(gap_nested[1,])
str(gap_nested[[1,]])   #error!

# for the first variable:
str(gap_nested[[1]])
str(gap_nested[1])  #In this case it is still a dataframe.

# To see how useful a nested dataframe can be, we will fit a model to each grouped data and
# then save each model in a new variable

LM <- lm(lifeExp ~ I(year-1950), data = gap_nested[[1, "data"]])
library(broom)
# 'broom::tidy()', To extract coefficient part of a linear model
# 'broom::augment()', To find residuals, fitted values, etc of the model
# 'broom::glance()', To find out summary statistics for entire regression of the model
# Following link might be helpful: http://borncity.com/win/2016/07/08/check-and-repair-windows-system-files-and-component-store/

tidy(LM)
augment(LM)
glance(LM)

# To find a linear model for data related to each country more efficiently, we use nested dataframe
# and 'purrr::map()'

le_vs_year <- function(df) lm(lifeExp ~ I(year - 1950), data = df)
(LM1 <- le_vs_year(gap_nested[[1,"data"]]))
tidy(LM1)
View(augment(LM1))
glance(LM1)

map(gap_nested[["data"]][1:2],le_vs_year)
(fit <- map(gap_nested[["data"]],le_vs_year))

(gap_nested4 <- mutate(gap_nested, fit = map(gap_nested[["data"]], le_vs_year)))

map(gap_nested4[["fit"]], tidy)

# 'unnest()': to extract information from nested data frame
# Variables being unnest should contain dataframes as elements
(gapnested5 <- gap_nested %>%
	mutate(fit = map(data, le_vs_year),
				 tidy = map(fit, tidy)) %>% 
	unnest(tidy))
# Now its time to tidy the derived dataframe
(coeffs <- gapnested5 %>% 
	select(continent:estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept =`(Intercept)`, slope = `I(year - 1950)`)) 

coeffs %>%
	select(-continent, -country) %>% 
	summary()

gapnested5 %>% 
	ggplot(aes(x = estimate)) + geom_density() + facet_wrap( ~ term, scales = "free") + geom_rug()
