getwd()
setwd(paste0(getwd(),"/", "cm011"))
rm(list = ls())
library(tidyverse)
library(forcats)
# 'system.file()'Finds the full file names of files in packages etc.
(gap_tsv <- system.file("gapminder.tsv", package = "gapminder"))
gap_tsv
gapminder <- read_tsv(gap_tsv)
str(gapminder, give.attr = FALSE)
# read_tsv() is a reader specific for tab delimited files.
# 'read_delim()' is a more general reader. You can define the delim in this function

# Difference between 'readr' functions and base R:
# 'readr' does not convert characters to factors. Thus, we can do it manually to take more control
# of it
gapminder_life_exp <- gapminder %>% 
	group_by(continent, country) %>% 
	summarise(life_exp = max(lifeExp)) %>% 
	ungroup() %>% 
	mutate(country = factor(country),
				 continent = factor(continent))

str(gapminder_life_exp, give.attr = FALSE)
levels(gapminder_life_exp$country)
# diff between "write_csv" and "write.csv": In the 'write.csv' we see many redundant quotes and row names

path=paste0(getwd(),"/","gapminder_life_exp.csv")
write_csv(gapminder_life_exp, "gapminder_life_exp.csv")
paste0(getwd(),"/","gapminder.life.exp.csv")
write.csv(gapminder_life_exp, "gapminder.life.exp.csv")

read_csv("gapminder_life_exp.csv")
read_csv("gapminder.life.exp.csv")

a <- read.csv("gapminder_life_exp.csv")
b <- read.csv("gapminder.life.exp.csv")
str(a)
str(b)

View(read_csv("gapminder_life_exp.csv"))
View(read.csv("gapminder.life.exp.csv"))
# Main difference b/w 'read_csv' and 'read.csv':
# In 'read_csv' all factors are converted to characters; characters are maintained in their natures
# In 'read.csv' all characters are converted to factors
# We can not maintain the factor orders; thus 'read_csv is more preferrable'

View(read_csv("gapminder_life_exp.csv"))

x <- c("a","b", "c")
y <- as.factor(c("x", "y", "z"))
DF <- data.frame(X = I(x), Y = I(y))
str(DF)
write_csv(DF, "D_F.csv")
write.csv(DF, "D.F.csv")

read_csv("D_F.csv")
str(read_csv("D_F.csv"), give.attr = FALSE)

read_csv("D.F.csv")
str(read.csv("D.F.csv"))
# Reordering Factors

str(gapminder_life_exp$country)
head(levels(gapminder_life_exp$country))
gap_life_exp <- gapminder_life_exp %>% 
	mutate(country = fct_reorder(country, life_exp))

head(levels(gap_life_exp$country))

country_levels <- tibble(original = levels(gap_life_exp$country))


write.csv(gap_life_exp, "gap.life.exp.2.csv")

csv <- read.csv("gap.life.exp.2.csv") %>% 
	.$country

country_levels <- mutate(country_levels, via_csv = levels(csv))

# Now I want to explore a method to save the data frame in a way that new factor orders are kept
# There are two methods: 1) saveRDS 2) dput
# 1)saveRDS:
saveRDS(gap_life_exp, "gap_life_exp.rds")


# '.rds' is a binary file
# It is nto readable through Github
# It is also handy for a no-rectangular dataset (like a fitted regression model)

# To see that country levels are the same after loading .rds and before saving it
rm(gap_life_exp)
gap_life_exp <- readRDS("gap_life_exp.rds")
# No changes in order of dataset
head(gap_life_exp$country)
head(levels(gap_life_exp$country))
country_levels <- country_levels %>% 
	mutate(via_rds = levels(gap_life_exp$country))

rm(gap_life_exp)
gap_life_exp <- read_csv("gapminder_life_exp.csv") %>% 
	mutate(country = factor(country))

country_levels <- country_levels %>% 
	mutate(via_csv = (levels(gap_life_exp$country)))

# 'dput()' and 'dget()'
#  it creates a plain text representation of an R object
# which still manages to be quite opaque. 

# 'dput()' creates an R-specific-but-not-binary representation.
rm(gap_life_exp)
gap_life_exp <- readRDS("gap_life_exp.rds")
dput(gap_life_exp, "gap_life_exp_dput.txt")
gap_life_exp_dput <- dget("gap_life_exp_dput.txt")
country_levels %>% 
	mutate(via_dput = levels(gap_life_exp_dput$country))
