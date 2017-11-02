rm(list = ls())
library(gapminder)
library(tidyverse)
library(readr)
superheroes <- "
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
publishers <- "
  publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"

publishers2 <- "
  publisher, yr_founded
Marvel,       1939
Image,       1992
"
superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)
publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)
publishers2 <- read_csv(publishers2, trim_ws = TRUE, skip =1)


superheroes
publishers
ijsp <- inner_join(superheroes, publishers)
#Just the rows of 'x' having matching values with 'y' are repeted in the new dataset
#'inner_join()' duplicates rows 'x' having matching with 'y' (one row for each matching).
#'Hellboy' doesnt exist in the output since its data doesnt exist in 'publishers'
#'inner_join()' is a mutating join
ijsp2 <- inner_join(publishers, superheroes)
# 'semi_join()'
# Return all rows from 'x' having matching values with 'y'
# But the variables of 'y' are not added
# differnce with 'inner_join()': 
# 'inner_join()' duplicates any row in 'x' matching with 'y'.
#  while 'semi_join()' never duplicate rows of 'x' (just one row for a row matching with 'y')
# 'semi_join()' is a filtering join

sjsp <- semi_join(superheroes, publishers)
#Here, results are similar to 'inner_join()' but only the variable of 'superheroes' are
# included
sjsp2 <- semi_join(publishers, superheroes)
#From 'sjsp2' it is visible that it doesn't duplicate any rows of 'x'
# It just looks through 'x' and finds rows having same values in 'y'.

#'left_join()':
# returns all rows from 'x' and all columns from both 'x' and 'y'.
# It is a mutating join, duplicates the row for each matching (one row for each matching)

Ljsp <- left_join(superheroes, publishers)
#'N/A' at the last row is the obvious difference b/w 'inner_join()' and 'left_join()'
#'left_join() duplicates any row of the first data set if there are multiole matches between
#'x and y
Ljsp2 <- left_join(publishers, superheroes)

#  anti_join():
# Is the reverse of semi_join()
# Just keeps the rows of 'x' not matching with 'y'
# It is a filtering join
ajsp <- anti_join(superheroes, publishers)
ajsp2 <- anti_join(publishers, superheroes)


# 'full_join()': 
# Renders all rows and all columns in both 'x' and 'y'
# If there isn't any value it renders N/A
# It gives the largest possible dataset
fjsp <- full_join(publishers, superheroes)
fjsp2 <- full_join(superheroes, publishers)
