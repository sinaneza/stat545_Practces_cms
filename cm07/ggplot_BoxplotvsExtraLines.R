rm(list = ls())
library(gapminder)
library(tidyverse)
library(knitr)

LifeExp_Mean <- gapminder %>%
  group_by(continent,year) %>%
  summarize(Weighted_Mean_lifeExpbyPop = weighted.mean(lifeExp, pop),
  					Average_lifeExp = mean(lifeExp))

#Data is filtered to Africa and arranged in ascending order of year
Gapminder_Africa <- filter(gapminder,continent=="Africa") %>% 
  arrange(year)

#statistical summary of gdpPercap in Africa
Africa_gdpPercap_summary <- summary(Gapminder_Africa$gdpPercap) 
Africa_gdpPercap_summary %>% 
	kable("markdown")

# To calculate weighted mean of lifeExp in Africa special yaer
LifeExp_Mean_Africa <- LifeExp_Mean %>% 
  filter(continent=="Africa")
LifeExp_Mean_Africa %>% kable("markdown")

#Drawing graph showing the tendency of lifeExp by passing time in Africa

##2) Box plot weighted mean of life expactancy in Africa
p_Africa_box <- ggplot(Gapminder_Africa,aes(x=as.factor(year),y=lifeExp))

p_Africa_box +
	geom_boxplot() +
	geom_line(data = LifeExp_Mean_Africa,
						aes(x=as.factor(year), y=Weighted_Mean_lifeExpbyPop),
						group=1, colour="#ff3933") +
	geom_line(data = LifeExp_Mean_Africa,
					aes(x = as.factor(year),y=Average_lifeExp),
					group=1, colour="#3342ff")
# Another way of what I did previously
p_Africa_box2 <- ggplot(Gapminder_Africa)
p_Africa_box2 + geom_boxplot(aes(x=as.factor(year), y=lifeExp))+geom_line(data = LifeExp_Mean_Africa,
																																					aes(x=as.factor(year), y=Average_lifeExp,group=1,colour="#3342ff"))
