# Writing figures to files:
# 1) Vector Graphics: PDF, postscripts, SVG, the quality of the figure doesn't change when we zoom in
# 2)Rasters: PNG, JPG, BMP, GIF: Pixel based graphs whose qualitiy terrifies after zooming in

library(gapminder)
library(tidyverse)

p <- gapminder %>% 
	ggplot(aes(x = year, y = lifeExp)) + geom_jitter()
# 'scale' actually changes the size of the plot
# As a side effect, it changes the relstive size of axis label and title
# When scale < 1, various plot elements will be bigger relative to the plotting area;
# when scale > 1, these elements will be smaller
# scale = 0.8 is generally suitable
p1 <- p + ggtitle("scale = 0.6")
p1
p2 <- p + ggtitle("scale = 2")
p2
paste0(getwd(), "/","fig-io-practice-scale-0.3.png")
ggsave(paste0(getwd(), "/", "cm013", "/" ,"fig-io-practice-scale-0.3.png"), p1, scale = 0.6)

ggsave(paste0(getwd(), "/", "cm013", "/" ,"fig-io-practice-scale-2.png"), p2, scale =2)

p3 <- ggsave()

p3 <- p + theme_grey(base_size = 20) + ggtitle("base_size = 20")
p3
p4 <- p +theme_grey(base_size = 3) + ggtitle("base_size = 3")
p4

# base_size defines of 'theme' defines the text font size
# It is an argument to various functions that set the theme elements 
# 'base_size<12'shrinks text elements
# 'base_size>12' expands text elements
ggsave(paste0(getwd(), "/", "cm013", "/", "fig-io-practice-base-size-20.png"),p3)
ggsave(paste0(getwd(), "/", "cm013", "/", "fig-io-practice-base-size-3.png"),p4)


list.files(path = paste0(getwd(),"/", "cm013"),pattern = "^fig-io*")
plot(1:10)


# Clean up our folder:
file.remove(list.files(path = paste0(getwd(), "/", "cm013"),
											pattern = "^fig-io*"))
