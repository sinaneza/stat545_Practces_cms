rm(list = ls())
library(gapminder)
library(tidyverse)

gapminder
str(gapminder)

gapminder
class(gapminder)
head(gapminder)
tail(gapminder)
iris
as_tibble(iris)
names(gapminder)
ncol(gapminder)
nrow(gapminder)
dim(gapminder)
summary(gapminder)
Afghanistan <- filter(gapminder, country == "Afghanistan")
Afghanistan
summary(Afghanistan)

#drawing some graphs
plot(lifeExp ~ year ,gapminder)
plot(lifeExp ~ year ,Afghanistan)
plot(lifeExp ~ year ,
		 filter(gapminder, country == "Iran"))
plot(lifeExp ~ gdpPercap, gapminder)
plot(lifeExp ~ log(gdpPercap), gapminder)
plot(lifeExp ~ log(gdpPercap), filter(gapminder, country == "Iran"))
plot(gdpPercap ~ year,
		 filter(gapminder, country == "Iran"))


str(gapminder)
str(gapminder$lifeExp)
class(gapminder$lifeExp)
summary(gapminder$lifeExp)
hist(gapminder$lifeExp)
summary(gapminder$gdpPercap)
hist(gapminder$gdpPercap)

str(gapminder$year)
class(gapminder$year)
summary(gapminder$year)
summary(gapminder$continent) #For non-numeric variables, summary just shows number of
														 #observations for each
table(gapminder$year)

gapminder$continent %>% 
	table() %>% 
	barplot()

p <- ggplot(filter(gapminder, continent!= "oceania"),
						aes(x = log10(gdpPercap) , y = lifeExp)) 
	# scale_x_continuous(limits = c(0,7))
# p <- p+scale_x_log10()
p+geom_point()
p + geom_point(aes(colour = continent))
p+geom_point(size=3, alpha=0.3) +
	geom_smooth(lwd=1, se= FALSE, method= "lm")

p+geom_point(size=3, alpha=0.3)+ 
	geom_smooth(lwd=3, se= TRUE, method= "auto")

p+geom_point(size=3, alpha=0.3)+ 
	geom_smooth(lwd=3, se= FALSE)

#Facet Wrap to draw separate plots for each continent

p2 <- ggplot(filter(gapminder, continent != "oceania"),
			 aes(x = gdpPercap, y = lifeExp))
p2 <- p2 + scale_x_log10()
p2 + geom_point(size = 3, alpha = 0.3) +
	geom_smooth(lwd =1.5, se = FALSE) +
	facet_wrap(~ continent)

# Identifying value of prediction
p+geom_point(size=3, alpha=0.3) + 
	stat_smooth(aes(outfit = fit <<- ..y..),
							lwd=3, se= FALSE)

#Extrapolation by geom_smooth():
#Hint: Extrapolation is just possible by applying linear modelling method

p + geom_point(size = 3, alpha = 0.3) + 
	geom_smooth(se = FALSE) +
	geom_smooth(data = filter(gapminder, continent != "oceania"),
							method = "lm",formula = y ~ splines::bs(x, 4),
							se = FALSE, colour = "red", fullrange = TRUE) +
	xlim(min(log10(gapminder$gdpPercap)), 7)

#This approach didnt work for the polynomial function.
#However, it should work for line regression.
q <- p + geom_point(size = 3, alpha = 0.3) + 
	geom_smooth(se = FALSE) +
	geom_smooth(data = filter(gapminder, continent != "oceania"),
							aes(outfit = fit <<- ..y.., infit=i <<- ..x..),
							method = "lm", fullrange = TRUE,
							se = FALSE, colour = "red") +
	xlim(min(log10(gapminder$gdpPercap)), 7)
q
Perdict <- data_frame(x=i,y=fit)
ggplot_build(q)



