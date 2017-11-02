#1 str_detect(): detects an element in a character atomic vector. The result is logic atomic vector

#2 str_subset(): subsets a vector having elements with specific string; The start and end
# arguments can be vectorized

#4 str_sub(1,3): subsets each string in a vector from their 1st letter to 3rd

#5 str_split(): Splits two worded elements to two individual elements; its result is a list
# Two words should be separated by a character defined by 'pattern' argument.

#5-1) str_split_fixed(): useful when number of separated words are known. Compared to 'str_split()',
#it makes matrix rather than a list since the length of each variable is known.

#6 tidyr: separate(): If the to-be-split variable is inside a data frame
#7 str_length(): Counts number of letters in each string (element)

#8 str_c(sep = , collapse = ): Collapse the vector
# It collapses elements of a vector if just the 'collapse' argument is defined
# It combines two vectors and seperates elements of each by a character defined by 'sep' argument
# we can combine two vectors into one and then collaps the restult by defining both arguments

#9 'tidyr: unite()': To combine two variables in a dataframe.

#10 str_replace(): raeplaces pattern argument with the replacement one
#11 str_replace_na: replaces NA with defined caharacter in replacement argument

# 12 tidyr::replace_na(data, replace = list): replaces NAs in different variables of a dataframe
# with characters defined in a named list. Names of the list should be the same as names of variables
# in dataframe.






rm(list = ls())
library(tidyverse)
library(stringr)
fruit
grepl("fruit", fruit)
fruit[grepl("fruit", fruit)]
grepl(" ", fruit)
fruit[grepl(" ", fruit)]
# 'grepl()' is in basic R, an equivalent for 'str_detect()'
logic <- str_detect(fruit, "fruit")

(my_fruit <- str_subset(string = fruit, pattern = "fruit"))

(split <- str_split(string = my_fruit, pattern = " "))
str(split)

(split_fixed <- str_split_fixed(string = my_fruit, pattern = " ", n = 2))
str(split_fixed)

tibble(my_fruit) %>% 
	separate(my_fruit, into = c("pre", "post"), sep = " ")

# Find a vector having length of each element
str_length(my_fruit)
# differ from length(my_fruit)
length(my_fruit)

head(my_fruit) %>% 
	str_sub(1,3)

# For those functions accepting just vector as an input, with is pretty useful.
tibble(fruit) %>% 
	with(str_subset(fruit, "fruit"))


Fruit <- fruit %>% 
	head() %>% 
	tibble()
names(Fruit) <- c("fruit")

Fruit %>% 
	mutate(snip = str_sub(fruit, 1:6, 3:8))

tibble(fruit) %>% 
	head() %>% 
	mutate(snip = str_sub(fruit, 1, 3))

(x <- head(fruit, 3))
str_sub(x, 1,3) <- "AAA"
x

(y <- head(fruit, 3))
str_sub(y, 1,3) <- c("AAA", "BBB", "CCC")
y

head(fruit) %>% 
	str_c(collapse = " & " )

tibble(a = fruit[1:4], b = fruit[5:8]) %>% 
	with(str_c(a, b, sep = " & "))

str_c(fruit[1:4], fruit[5:8], sep = " & ")
str_c(fruit[1:4], fruit[5:8], sep = " & ", collapse = " , ")


tibble(a = fruit[1:4], b = fruit[5:8]) %>% 
	unite("comb",a, b, sep = " & ")

tibble(a = fruit[1:4], b = fruit[5:8]) %>% 
	unite("comb",a, b, sep = " & ", remove = FALSE)

my_fruit %>% 
	str_replace(pattern = "fruit", replacement = "THINGY")

melon <- fruit %>% 
	str_subset(pattern = "melon")

melon[2] <- NA
melon
str_replace_na(melon, "UNKNOWN MWLON")

tibble(melon = melon) %>% 
	replace_na(replace = list(melon = "UUNKNOWN MELON"))
## regexs:
# 1) '.': Any singlr character, other than  new line
# ex: "a.b": any string having "a" followed by any character and then "b".
# 2) '\n': New line
# NOTE: Regexes are case sensitive.

## Anchors: To define where expressions should be
# 1)'^d': Strings having "d" at first
# 2)'d$': Strings having "d" at last
# 3)Metacharacters: Specified by "\":
# 3-1)\b: matches the empty string at either edge of a word. 
# Don’t confuse it with ^ $ which marks the edge of a string.
# ex: "\\bn" boudary should be before "n" (string starting with "n" or having space before "n")
# ex: "n\\b": boudary should be after "n" (string ending with "n" or having space after "n")
# 3-2) \B: matches the empty string provided it is not at an edge of a word.
# Note: Both should be followed after another "\"

# Escaping: Some characters can not be included inside a double coatation, like, "'",
# so they should be preceded by "\". This is called escaping
# \': single quote. You don’t need to escape single quote inside a double-quoted string, so we can also use "'" in the previous example.
# \": double quote. Similarly, double quotes can be used inside a single-quoted string, i.e. '"'.
# \n: newline.
# \r: carriage return.
# \t: tab character.

# Quantifiers: Define the number of repetitions in the pattern.
# *: matches at least 0 times.
# +: matches at least 1 times.
# ?: matches at most 1 times.
# {n}: matches exactly n times.
# {n,}: matches at least n times.
# {n,m}: matches between n and m times.
# {,m}: matches atmost m times

## Operators:
# .: matches any single character, as shown in the first example.

# [...]: a character list, matches any one of the characters inside the square brackets. 

# We can also use  - inside the brackets to specify a range of characters.

# [^...]: an inverted character list, similar to [...],
# but matches any characters except those inside the square brackets.

# \: suppress the special meaning of metacharacters in regular expression,
# i.e.  $ * + . ? [ ] ^ { } | ( ) \, similar to its usage in escape sequences.
# Since \ itself needs to be escaped in R, 
# we need to escape these metacharacters with double backslash like \\$.

# |: an “or” operator, matches patterns on either side of the |.

# (...): grouping in regular expressions.
# This allows you to retrieve the bits that matched various parts of your regular expression
# so you can alter them or use them for building up a new string.
# Each group can then be refer using \\N, with N being the No. of (...) used. 
# This is called backreference.

## Character Classes: [:...:] or \c
# [:...:] in another square brackets and \.. should be followed after another \

# [:digit:] or \d: digits, 0 1 2 3 4 5 6 7 8 9, equivalent to [0-9].
# \D: non-digits, equivalent to [^0-9].
# [:lower:]: lower-case letters, equivalent to [a-z].
# [:upper:]: upper-case letters, equivalent to [A-Z].
# [:alpha:]: alphabetic characters, equivalent to [[:lower:][:upper:]] or [A-z].
# [:alnum:]: alphanumeric characters, equivalent to [[:alpha:][:digit:]] or [A-z0-9].
# \w: word characters, equivalent to [[:alnum:]_] or [A-z0-9_].
# \W: not word, equivalent to [^A-z0-9_].
# [:xdigit:]: hexadecimal digits (base 16), 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f, equivalent to  [0-9A-Fa-f].
# [:blank:]: blank characters, i.e. space and tab.
# [:space:]: space characters: tab, newline, vertical tab, form feed, carriage return, space.
# \s: space, ` `.
# \S: not space.
# [:punct:]: punctuation characters, ! " # $ % & ’ ( ) * + , - . / : ; < = > ? @ [  ] ^ _ ` { | } ~.
# [:graph:]: graphical (human readable) characters: equivalent to [[:alnum:][:punct:]].
# [:print:]: printable characters, equivalent to [[:alnum:][:punct:]\\s].
# [:cntrl:]: control characters, like \n or \r, [\x00-\x1F\x7F]


library(gapminder)
(country <- levels(gapminder$country))

str_subset(string = country, pattern = "i.a")
str_subset(string = country, pattern = "i.a$")

str_subset(fruit, "d")
str_subset(fruit, "^d")
str_subset(fruit, "melon")
str_subset(fruit, "\\bmelon")
str_subset(fruit, "\\Bmelon")

str_subset(country, "[nls]ia$")
str_subset(country, "[nls]ia$")

str_split_fixed(fruit, "[[:space:]]", 2)
str_split_fixed(fruit, "\\s", 2)

str_subset(country, "[[:punct:]]")
str_subset(fruit, "l.*e")
str_subset(fruit, "l.+e")
# 'intersect()' finds intersection between two sets
# 'setdiff()' finds differnces between two sets
list(match = intersect(str_subset(fruit, "l.*e"),
											 str_subset(fruit, "l.+e")),
		 no_match = setdiff(str_subset(fruit, "l.*e"),
		 									 str_subset(fruit, "l.+e")))

matches <- str_subset(fruit, "l.*e")

list(match = intersect(matches,
											 str_subset(fruit, "l.?e")),
		 no_match = setdiff(matches,
		 										 str_subset(fruit, "l.?e")))

list(match = intersect(matches,
											 str_subset(fruit, "le")),
		 no_match = setdiff(matches,
		 									 str_subset(fruit, "le")))

# There are some characters having special meaning in R strings, such as "*","?" (regexes)
# But some times it is required to use the sign, not the regex (for example "+" sign)
# the sign ("+" for example) is considered as string prior to be considered as regex.
# To prevent that we can use another"\"
# To print the real interpretation, use 'cat()' instead of 'print()'
print("a\nb")
cat("a\nb")
cat("Do you use \"airquotes\" very often?")
cat("before the newline\nafter the new line")

# Now I want to find countries having '.' in their names
str_subset(string = country, pattern = "\\.")

x <- c("whatevwer", "X is distributed U[0,1]")
str_subset(x, "[")
# error
str_subset(x, "\\[")


# 2014 link:
# grep(value = TRUE): Returns the characters
# grep(value = FALSE): Retuurn indices
# grepl():returns logical vector

string <- c("a", "ab", "acb", "accb", "acccb", "accccb")
str_subset(string, "a.b")
str_subset(string, "a.*b")
str_subset(string, "a.+b")
str_subset(string, "a.?b")
str_subset(string, "a.{2}b")
str_subset(string, "a.{2,}b")
str_subset(string, "a.{2,4}b")

str_subset(country, "ee")

(strings <- c("abcd", "cdab", "cabd", "c abd", "cab d"))
grep("ab",strings, value = TRUE)
grep("^ab", strings, value = TRUE)
grep("ab$", strings, value = TRUE)
grep("\\bab", strings, value = TRUE)
grep("ab\\b", strings, value = TRUE)
grep("\\Bab", strings, value = TRUE)
grep("ab\\B", strings, value = TRUE)

list <- list.files()
grep("\\.R", list, value = TRUE)
grep("\\.html", list, value = TRUE)

(strings <- c("^ab", "ab", "abc", "abd", "abe", "ab 12"))
grep("ab.", strings, value = TRUE)
grep("ab[c-e]", strings, value = TRUE)
grep("ab[^c]", strings, value = TRUE)
grep("^ab", strings, value = TRUE)
grep("\\^ab", strings, value = TRUE)
grep("abc|abd", strings, value = TRUE)
gsub("(ab) 12", "\\1 34", strings)
str_replace(strings, "(ab) 12", "\\1 34")

str_subset(country, "i|t|I|T$") %>% 
	str_subset("(land$)") %>% 
	str_replace("\\1", "LAND")

str_replace(country,"(i|I|t|T)land$", "\\1LAND") %>% 
	str_subset("(i|I|t|T)LAND$")

str_replace(country, "(i|t|I|T)(.*)land$", "\\1\\2LAND") %>% 
	str_subset("(i|I|t|T).*LAND")

# There are 2 types of regexes: 
# 1)POSIX extended regular expressions (default)
# 2)Perl-like regular expressions.
# 3)fixed (The written pattern is sconsidered litterally, not the regex)

# perl can be defined by 'perl = TRUE/FALSE' in R base functions and by 'perl()' in stringr
# fixed can be defined by 'fixed = TRUE/FALSE' in R base functions and by 'fixed()' in stringr

(strings <- c("Axbc", "A.bc"))
pattern <- "A.b"
grep(pattern, strings, value = TRUE)
grep(pattern, strings, value = TRUE, fixed = TRUE)
# Somehow similar to escaping

# By default, regexes are case sensitive unless it is mentioned.
# 'ignore.case = TRUE/FALSE' in R base functions and 'ignore.case()' in 'stringr'

(continent <- levels(gapminder$continent))

grep("o", continent, value = TRUE)
grep("o", continent, value = TRUE, ignore.case = TRUE)
