library(gapminder)
library(tidyverse)
gapminder %>%
  filter(country %in% c("Iran", "Canada", "Germany")) %>% 
  ggplot(aes(x=lifeExp,y=gdpPercap))+geom_point(aes(colour=country))

justthree <- gapminder %>% 
  filter(country %in% c("Iran", "Canada", "Germany"))
levels(justthree$country)

justthree$country <- justthree$country %>% 
  droplevels()  #droplevels is used to drop unused levels from a factor or, more commonly, from factors in a data frame.
levels(justthree$country)
nlevels(justthree$country)