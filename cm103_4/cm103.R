rm(list = ls())
a <- c(TRUE, FALSE, TRUE)
b <- c(10, 9, 8, 7, 6)
c <- c(1.1, 2.11, 3.38)
d <- letters[1:4]
e <- as.factor(letters[1:4])
# mode(): indicates the storage mode
# typeof(): indicates the type of the storgae mode. For numeric modes, either integer or double,
# For non-numeric modes, their type is the same as their mode.

mode(a)
class(a)
typeof(a)

mode(b)
class(b)
typeof(b)

mode(c)
class(c)
typeof(c)

mode(d)
class(d)
typeof(d)

mode(e)    #The storage mode of a factor is numeric
class(e)   #The type of that storage mode is integer
typeof(e)

is.numeric(a)
is.double(a)
is.integer(a)
is.character(a)
is.logical(a)

is.numeric(b)
is.double(b)
is.integer(b)
is.character(b)
is.logical(b)

is.numeric(c)
is.double(c)
is.integer(c)
is.character(c)
is.logical(c)

is.numeric(d)
is.double(d)
is.integer(d)
is.character(d)
is.logical(d)

d[0]
length(d)
d[c(4,1, 2, 3, 1)]

# Use 'L' to explicitly mention that a value is an integer
f <- c(5L, 6L, 1L, 10L)
mode(f)
typeof(f)
class(f)

library(tidyverse)
g <- data_frame(chr = letters[1:4], num = c(1:4))
mode(g)     #Saving mode of a data frame is a list.
typeof(g)
class(g)    #However, its class is dataframe

h <- list(chr = letters[1:4], num = c(1:4))
mode(h)
typeof(h)
class(h)

jMat <- outer(as.character(1:4), as.character(1:4),
							function(x, y) {
								paste0('x', x, y)
							})
mode(jMat)
typeof(jMat)
class(jMat)

as.logical(d)
as.numeric(d)
as.double(d)
as.integer(d)


(x <- list(1:3, c("four", "five")))
(y <- list(logical = TRUE, integer = 4L, double = 4 * 1.2, character = "character"))
(z <- list(letters[26:22], transcendental = c(pi, exp(1)), f = function(x) x^2))
# Elements of a list are not necessarily atomic vectors. They can be functions as well.
# When applying names, it is not essencisal for all elements of a list to have names.
# Some may have and some may not.

# Indexing an atomic vector:
# 1) By a logical vector. 
	# 1-a) It should be in the same length of the atomic vetor
	# 1-b) Otherwise, it would repeat the logical vector to achieve the length of vector
	# which is undr indexing

# 2) By a positive signed integer vector (to keep those indexes)
# 3) By a negative signed integer vector (to eliminate those indexes)
# 3) By a character vector(for indexing a named vector)

# Indexing a list
# 1) Indexing by a single bracket, resulting in a list
# 2) Indexing by two brackets, resulting in a naked element
# 3) Indexing by $, similar to two brackets indexing, but just indexing by names (characters)
x[1]
x[[1]]
str(x[[1]])
x[[1]][1]
x[[1]][[1]]
str(x[[1]][1])
str(x[[1]][[1]])

x[c(FALSE, TRUE)]
y["double"]
y[["double"]]
y$double
y[[3]]
z$transcendental

my_vec <- c(a = 1, b = 2, c = 3)
my_list <- list(a = 1, b = 2, c = 3)
my_vec[2]
str(my_vec[2])
my_vec[[2]]   #In double bracket case, we can not see the assigned name, so it is just the naked element
my_vec["b"]
my_vec[["b"]]
my_vec[[c(2,3)]]  #error!

my_list[["b"]]
my_list[c("b", "c")]
my_list[[c("b", "c")]]  #error! it can not index two elements simultaneously as an atomic vector (non-list)
my_list[c("b", "c")]  #successful attempt, since the subset is still a list

c
k <- rep(NA_real_, length(c))
for ( i in seq_along(c)){
	k[i] <- exp(c[i])
}
# But, there is no need to for loop, since R is a vector based language
# It can be easily done as follow:
L <- exp(c) 
identical(k,L)
all.equal(k,L)

# But unlike vectors, R doesn't apply a function to all elements of a list.
# It doesn't make sence since lists are heterogeneous object and it is unknown if a function
# is applicable to all elements.

# To do the same thing, 'map()' is a useful function.
# 'purrr::map()' applies a function to all elements of a list.
# It is said: "The function is mappd over the list"
M <- as.list(c)
exp(M)
library(purrr)
map (M, exp)

# eauivalent to the following code
length(M)
N <- as.list(rep(NA_real_, length(M)))

for (i in seq_along(M)) {
	N[[i]] <- exp(M[[i]])
}
N

# Note: lapply(X, Fun): applies a function to all elements of an atomic vector or a list.
# 'lapply()' is an R basic counterpart of "purrr::map()".

# "ghit" package, useful for installation from github
# "repurrrsive pacakage, to have some recursive lists for the purpose of practice"
# "purrr" is useful to deal with lists (applying functions and ...)('map()')
# "listviewer" package, useful to view recursive lists more efficienly. ('jsonedit()')
library(ghit)
library(repurrrsive)
library(purrr)
library(listviewer)
library(tidyverse)

# map is useful to apply a function to  either all elements of a list or a vector;
# eqivalent to lapply and sapply.
#Both 'purr::map()' and "lapply" throw a list as outpt
# However, 'saplly()' throws an atomic vector.
map(c(9.16,25,49), sqrt)
lapply(c(9.16,25,49), sqrt)
sapply(c(9.16,25,49), sqrt)
sapply(list(9.16,25,49), sqrt)

# "gh_users" is a list in "repurrrsive" package. containing some information on some github users.

gh_users

str(gh_users)

# we use 'jsonedit(list, mode = "view")' to see the list more efficiently
jsonedit(gh_users, mode = "view")

str(gh_users, max.level =1)  #To just see the structure of elements in the forst layer of the list.
str(gh_users, list.len = 6)  #No more than 6 lists can be seen in each layer of the list

## Classical methods of indexing a list:
# 1) single bracket
# 2) double brackets
# 3) $

## Indexing by "purrr: map()", indexing all lists in the first layer of a list:
# 1)Defining function inside "map()"
# 2) map(list, object's name)
# 3) map(list, object's number in the list)
# 4)map_chr(), map_dbl, map_int, map_lgl, ... to get a vector instead of a list
	# Note: "map_chr()" is just uselful for charactr vectors, "map_dbl()" for double precision vectors
	# and so on
# 5) mapping 'magrittr::extract()' to the lis. arguments of mapped function come after the
	# function call in 'map()
	# map(list, extract, c("a", "b", "c"))
	#"magrittr::extract2()" acts like "[["
# 6) converting '[' or '[[' to function using '`'.
	# Note: "`" is different from "'"
	# map(list, `[`, c("a", "b", "c"))
## Note1: "map()" throws a list as an out out. Do not anticipate vectors.
## Note2: methods 5 and 6 are useful when we want to index mutiple variables of lists
gh_users[5][c(1, 2, 6, 18, 21, 24)]  #It didnt fulfil our request, since "gh_users[5]" is still a list containing justb 5th element.
gh_users[[5]][c(1, 2, 6, 18, 21, 24)]

jsonedit(gh_users[5], mode = "view")
jsonedit(gh_users[[5]], mode = "view")

# We can define a function inside "purrr:map()".
L1 <- map(gh_users, function(x) x[["name"]])
L2 <- list(gh_users[[1]][["name"]],gh_users[[2]][["name"]], gh_users[[3]][["name"]],
					 gh_users[[4]][["name"]], gh_users[[5]][["name"]], gh_users[[6]][["name"]])
all.equal(L1,L2)
identical(L1, L2)
jsonedit(L1, mode = "view")
jsonedit(L2, mode = "view")


map(gh_users, "login")
map(gh_users, 1)

map(gh_users, "name")
map(gh_users, 18)

map(gh_users, c("name", "login"))   #Didn't work, meyhods 5 and 6 are helpful

# piping is also helpful:
gh_users %>% 
map("login")

gh_users %>% 
	map("created_at")
which(names(gh_users[[1]]) == "created_at")
gh_users %>% 
	map(29)

map(gh_users, "egg plant")
map(gh_users, 35)

map(gh_users, "login") %>% 
	map(is_character)

map_chr(gh_users, "login")
map_int(gh_users, "login") #The element is not an integer, it is a character


# lapply keeps outout as list
# sapply converts it to atomic vector if it is possible.
lapply(gh_users, "login") #lapply and sapply are not as handy as map. They need a function to be defined.

lapply(gh_users,`[`, "login")
lapply(gh_users,`[[`, "login")
lapply(gh_users,`[[`, "login")
lapply(gh_users, function(x) x[["login"]])
jsonedit(lapply(gh_users,`[`, "login"), mode = "view")
jsonedit(lapply(gh_users,`[[`, "login"), mode = "view")
str(lapply(gh_users,`[[`, "login"), mode = "view")

sapply(gh_users,`[`, "login")
str(sapply(gh_users,`[`, "login"))
sapply(gh_users,`[[`, "login")
sapply(gh_users, function(x) x[["login"]])
jsonedit(sapply(gh_users,`[`, "login"), mode = "view")

jsonedit(gh_users, mode = "view")

map(gh_users, "site_admin")

map_lgl(gh_users, "site_admin")

map(gh_users, "id") %>% 
	map(class)

map_int(gh_users, "id")

map(gh_users, "followers")
map_int(gh_users, "followers")
jsonedit(map(gh_users, "followers"), mode = "view")

map(gh_users, "company") %>% 
	map_lgl(is_character)

# What about indexing multiple objects from a list
gh_users[[1]][c("name", "login", "id", "location")]
gh_users[[2]][c("name", "login", "id", "location")]

# What if I want to index multiople objects from the lists in the first layer of a list? 
(A <- map(gh_users, `[`, c("name", "login", "id", "location" )))
library(magrittr)
(B <- map(gh_users, extract, c("name", "login", "id", "location")))
all.equal(A,B)
jsonedit(A, mode = "view")
jsonedit(gh_users, mode = "view")
