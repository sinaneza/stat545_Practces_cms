# 'par()' sets all base R graphical parameters
# storing it in a new variable (opar) to have the option of resetting to default options by 'par(opar)'
# to recall the newly defined setting, we have to assign plot to opar
# pch = 19 relates to solid circular representation
rm(list = ls())
opar <- par(pch = 19)

# There is a specific chunk for changing default plotting options in Rmarkdown:


library(gapminder)
library(tidyverse)

j_country <- c("Eritrea", "Nepal", "Chad", "Jamaica", "Cuba", "Costa Rica", "Germany", "Norway")
jDat <- gapminder %>% 
	filter(year == 2007,
				 country %in% j_country)

j_xlim <- c(460, 60000)
j_ylim <- c(47, 82)

plot(lifeExp ~ gdpPercap, jDat, log = 'x', xlim = j_xlim, ylim = j_ylim, main = "Start Your Engine")

# We can specify colours (one or more)
# If number of colours are less than number of points, these colours are repeated

plot(lifeExp ~ gdpPercap, jDat, 
		 log = 'x', xlim = j_xlim, ylim = j_ylim,
		 col = "red", main = 'col = "red"')

# The case of two colours and repetition of them
plot(lifeExp ~ gdpPercap, jDat,
		 log = 'x', xlim = j_xlim , ylim = j_ylim,
		 col = c("blue", "orange"),
		 main = 'col = c("blue", "orange")')

# It is also possible to specify a colour by a number, that is indexing from current palette
# current palette can be investigated through 'palette()'
palette()
n_c <- nrow(jDat)

plot(lifeExp ~ gdpPercap, jDat, 
		 log = 'x', x_lim = j_xlim, y_lim = j_ylim, 
		 col = 1:n_c, main = paste0('col = 1:', n_c))
# To write under each point
with(jDat, text(x = gdpPercap, y = lifeExp, pos = 1))

plot(lifeExp ~ gdpPercap, jDat,
		 log = 'x', xlim = j_xlim, ylim = j_ylim,
		 col = 1:n_c, main = 'The default palette()')
with(jDat, text (x = gdpPercap, y = lifeExp, 
								 labels = palette(), pos = rep(c(1,3,1), c(5,1,2))))

plot(lifeExp ~ gdpPercap, jDat,
		 log = 'x', xlim = j_xlim, ylim = j_ylim,
		 col = 1:n_c, main = 'Country Names')
with(jDat,
		 text(x= gdpPercap, y = lifeExp,
		 		 labels = jDat$country, pos =rep(c(1, 4, 2, 1, 3), c(1, 1, 1, 4, 1))))

# It is also possible to select custom colours
j_colours <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3',
							 'mediumorchid2', 'turquoise3', 'wheat4', 'slategray2')
plot(lifeExp ~ gdpPercap, jDat,
		 log = 'x', xlim = j_xlim, ylim = j_ylim,
		 col = j_colours, main = 'Custom Colours!')
with(jDat,
		 text(x = gdpPercap, y = lifeExp, labels = j_colours, pos = rep(c(1, 3, 1), c(5, 1, 2))))
# To see colours available in R
colours()
head(colours())
tail(colours())
# It is not always desireable to find and set the custom colours manually.
# There are some packages helping us with it
# 1) RColourBrewer
# install.packages("RColorBrewer")
library(RColorBrewer)
# To load all and see all available colours
display.brewer.all()
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
display.brewer.all(type = "div")

# To see colours in each palette, knowing the discussed palette name
display.brewer.pal(n=8 ,name = "Dark2")
display.brewer.pal(n=3, name = "Dark2")
# To identify the number (code) of each palette
dark2_brewer_colours<- brewer.pal(n=8, name = "Dark2")

plot(lifeExp ~ gdpPercap, jDat,
		 log = 'x', xlim = j_xlim, ylim = j_ylim,
		 col = dark2_brewer_colours, main = 'Brewer Col')
with(jDat, text(x = gdpPercap, y = lifeExp,
								labels = dark2_brewer_colours ,pos = rep(c(1,4,1), c(1,1,6))))
# 2)viridis
# Viridis is a package enabling us to 
# install.packages("viridis")
# install.packages("viridisLite")
library(viridisLite)
library(viridis)
viridis(n=200, alpha =1, begin = 0, end = 1, direction = 1, option = "D")
library(scales)
viridis_pal(alpha =1, begin = 0, end = 1, direction = 1, option = "D") %>% 
	show_col()

# viridis is usable nicely in ggplot:
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)),
			 aes(x = x, y= y)) + geom_hex() + coord_fixed() + scale_fill_viridis()+theme_bw()

x <- seq(-8*pi, 8*pi, len = 40)
y <- seq(-8*pi, 8*pi, len = 40)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), color.palette = viridis, axes = FALSE)
filled.contour(cos(r^2)*exp(-r/(2*pi)))
# Upto Hexadecimal

# install.packages("dichromat")
library(dichromat)
# dichromat is a package for colours suitable for colour blind people

# Now its the time to reverse settings applied by opar to the default setting
par(opar)
