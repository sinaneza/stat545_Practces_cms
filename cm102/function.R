# Writing a function

rm(list = ls())
library(gapminder)
library(tidyverse)
str(gapminder)

## Diiferent methods to calculate 'max - min'
min(gapminder$lifeExp)
max(gapminder$lifeExp)
max(gapminder$lifeExp)-min(gapminder$lifeExp)

# 'range()' gives a vevtor containing maximum and mnimum of its input
range(gapminder$lifeExp)
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]

gapminder %>%
	with(max(lifeExp) - min(lifeExp))

with(gapminder, range(lifeExp)[2] - range(lifeExp)[1])
# diff(x, lag, diff):
# If 'diff = 1', x(1+lag:n)-x(1:lag-n)
# If diff is other than 1, the algorithm is repeated recursively
# diff(x, lag, diff = 2):
# a <- x(1+lag:n)-x(1:n-lag), ans <- a(1+lag:n)-a(1:n-lag)

diff(range(gapminder$lifeExp))

max_minus_min <- function(x) {
	max(x)-min(x)
}


max_minus_min(gapminder$lifeExp)
max_minus_min(gapminder$pop)

# Lets test weird staff o  our newly designed function
max_minus_min(gapminder)
max_minus_min(gapminder$country)
max_minus_min("eggplants are purple")
# Nice, it resulted in error

# What if I use numeric data frame or logic vector:
max_minus_min(gapminder[c("lifeExp", "pop", "gdpPercap")])
max_minus_min(c(TRUE, FALSE, TRUE,TRUE))
# Damn it! It rann through

# To prevent this trouble, check the validity of arguments
mmm <- function(x) {
	stopifnot(is.numeric(x))
	max(x)-min(x)
}

mmm(gapminder$lifeExp)
mmm(gapminder[c("lifeExp", "pop", "gdpPercap")])
mmm(c(TRUE, FALSE, TRUE,TRUE))

# To improve error message, we use 'if'
mmm2 <- function(x) {
	if(!is.numeric(x)){
		stop('I am so sorry, but this function only works for numeric input!\n',
				 'You have provided an object of class: ', class(x)[1])
	}
	max(x)-min(x)
	}
mmm2(c(TRUE, FALSE, TRUE,TRUE))

mmm2


## Methods to extract quantiles:
#1) 'quantile()' do this and 'prob' argument defines the probability of the extracted quantile
# By default, 'quantile' extracts the 3 quartile of data
# 2) 'boxplot()' by defining 'plot = FALSE'. This just defines 3 quartiles

quantile(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = 0.5)
quantile(gapminder$lifeExp, probs = c(0.25, 0.5))
quantile(gapminder$lifeExp, probs = c(0.20, 0.40, 0.60, 0.80))
boxplot(gapminder$lifeExp, plot = FALSE)$stat

the_probs <- c(0.25, 0.5)
the_quantiles <- quantile(gapminder$lifeExp, the_probs)
max(the_quantiles)-min(the_quantiles)

## Passing arguments of your function as arguments of a built-in function.
qdiff1 <- function(x, probs) {
	stopifnot(is.numeric(x))
	the_quantiles <- quantile(x, probs)
	max(the_quantiles) - min(the_quantiles)
}

qdiff1(x = gapminder$lifeExp, probs = c(0.25, 0.5))
# We get an error if the probs is remanied undefined.
qdiff1(x = gapminder$lifeExp)
qdiff1(x = gapminder$lifeExp, probs = c(0.25, 0.75))
# IQR(): finds the differences between first and third quartiles
IQR(x = gapminder$lifeExp)

qdiff1(x = gapminder$lifeExp, probs = c(0,1))
mmm(gapminder$lifeExp)

# Note: we can name larguments of a function in any names that we want, but it would be better
# for names to be more indicative. preferrably the same as names of built-in functions arguments
# if they are going to be used.


# By default, the funtion returns its last line as an output.
# We can use 'return()' to indicate the line that should be returned.
# Return line must be the last line, otherwise other other variables wouldn't be updated. 

qdiff2 <- function(x, probs){
	stopifnot(is.numeric(x))
	the_quantiles <- quantile(x, probs)
	gapminder$lifeExp
}
qdiff2(x = gapminder$lifeExp, probs = c(0.25, 0.75))
qdiff1(x = gapminder$lifeExp, probs = c(0.25, 0.75))
qdiff2(x = gapminder, probs = c(0.25, 0.75))

# As mentioned previously, R would have thrown error if the 'probs' was not sepecified
# To define default values, we should define them in the beginning of the function

qdiff3 <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1)){
	stopifnot(is.numeric(x))
	the_quantiles <- quantile(x, probs)
	return(max(the_quantiles) - min(the_quantiles))
}

qdiff3(x = gapminder$lifeExp)
mmm((x = gapminder$lifeExp))
qdiff3(x = gapminder$lifeExp, probs = c(0.25, 0.75))

# 'na.rm = TRUE/FALSE' is for NA management. most functions have this argument to know how to
# deal with "NA" elements. by default it is "FALSE" and in the case of having NA elements, the 

# function throws an error
z <- gapminder$lifeExp
z[3] <- NA
quantile(gapminder$lifeExp)
quanatile(z)
quantile(z, na.rm =TRUE)

# One way to adjust our newly defined function to NA management is just to set 'na.rm'argument in the
# built in function to 'TRUE' or 'FALSE'.
# This is not a good idea since we are restricting the user to the defined value.

qdiff4 <- function(x, probs){
	stopifnot(is.numeric(x))
	the_quntiles <- quantile(x = x, probs = probs, na.rm = TRUE)
	return(max(the_quntiles)-min(the_quntiles))
}
qdiff4(z, probs= c(0.25, 0.75))

# Better way, to define an argument as 'na.rm' in the original function.
qdiff5 <- function(x, probs, na.rm = FALSE){
	stopifnot(is.numeric(x))
	the_quntiles <- quantile(x = x, probs = probs, na.rm = na.rm)
	return(max(the_quntiles)-min(the_quntiles))
}

qdiff5(z)
qdiff5(z, probs = c(0.25 , 0.75), na.rm = TRUE)




# '...' is used to pass arguments to buit-in function
qdiff6 <- function(x, ...) {
	stopifnot(is.numeric(x))
	the_quantiles <- quantile(x, ...)
	return(max(the_quantiles) - min(the_quantiles))
}
qdiff6(gapminder$lifeExp)
qdiff6(gapminder$lifeExp, probs = c(0.25, 0.75))
qdiff6(z, probs = c(0.25, 0.75))
qdiff6(z, probs = c(0.25, 0.75), na.rm = TRUE)

# There are 9 algorithms of calculating quantiles, specified by 'type' argument of 'quantile()'.
# They use different ways to find quantiles if they are not exactly onne of the elements of 'x'

(a <- qdiff6(z, probs = c(0.25, 0.75), na.rm = TRUE, type = 2))
(b <- qdiff6(z, probs = c(0.25, 0.75), na.rm = TRUE, type = 9))

# all 'probs', 'na.rm', and 'type' are transferred to 'quantile()' by two '...' in both functions.

# 'all.equal(target, current)' gives the mean relative difference of two inputs
all.equal(a, b)

# 'testthat' is a package doing automatic tests on newly defined functions
# The under-test-function may be a function in a package
# test_that(description, code)
# If you wnt to test equality of result with a value use 'expect_equal()'
# If you expect an error, use 'expect_error()'
# 'expect_that()' uses a condidition as second argument, such as 'equals()' or 'is_identical_to()'

library(testthat)

test_that('Invalid args are detected',{
	expect_error(qdiff6("eggplants are purple"))
	expect_error(qdiff6(iris))
})

test_that('NA handling works', {
	expect_error(qdiff6(c(1:5, NA)))
	expect_error(qdiff6(c(1:5, NA), na.rm = FALSE))
	expect_equal(qdiff6(c(1:5, NA), na.rm = TRUE), 4)
	})

qdiff_no_NA <- function(x, probs = c(0,1)){
	the_quantiles <- quantile(x, probs)
	return(max(the_quantiles)-min(the_quantiles))
}
qdiff_no_NA(c(1:5))
qdiff_no_NA(c(1:5, NA))

test_that('NA handling works', {
	expect_that(qdiff_no_NA(c(1:5)), equals(4))
})

test_that('NA handling works', {
	expect_that(qdiff_no_NA(c(1:5, NA)), equals(4))
})
