library(gapminder)
library(tidyverse)

mini_gap <- gapminder %>% 
	filter(country %in% c("Belgium", "Canada", "United States", "Mexico"),
				 year > 2000) %>% 
	select(-pop, -gdpPercap) %>% 
	droplevels()
mini_gap

food <- tribble(
	~ country,    ~ food,
	"Belgium",  "waffle",
	"Canada", "poutine",
	"United States", "Twinkie"
)
food

# match(x, table) reports where the values in the key x appear in the lookup variable table.

match(x = mini_gap$country, table = food$country)

(indices <- match(x = mini_gap$country, table = food$country))
add_column(food[indices, ], x = mini_gap$country)
?add_column

mini_gap %>% 
	mutate(food = food$food[indices])

# Using Join Functions are more convenient if the final result is just under interest.

(food_vec <- setNames(food$food, food$country))

mini_gap %>% 
	mutate(food = food_vec[country])
# The result is not satisfactory
# Reason: country was a factor
# To suppress it, it should be converted to chracter

unclass(mini_gap$country)

mini_gap %>% 
	mutate(food = food_vec[as.character(country)])
