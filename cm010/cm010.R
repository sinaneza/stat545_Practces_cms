
x <- 3*4
is.vector(x)
length(x)

x[2] <- 50
x
x[5]<- 3
x
# Undefined elements are remained as N/A
rnorm(n = 5, mean = 10)
# in 'rnorm()' 'mean' and 'sd' can be vectors, so be careful
rnorm(n = 5, mean = 10^(0:4), sd = 2)

# When multiple types of objects are in a vector the vector's class changes to the structure of 
# one of the elements regarding the following hierarchy
# 1) Characters, 2) Numerics, 3) Logical
a <- c(TRUE, 1, "Sina")
a2 <- c(TRUE, 1, 2)
str(a)
mode(a)
str(a2)
class(a)
b <- as.factor("Nez")
a2 <- c(TRUE, 1, "Sina",b)

n <- 8
set.seed(1)
# 'round(a,2)' rounds a to two decimals
w <- round(rnorm(8), 2)
# seq_along() creates a sequential vector in the length of its input. its step would be 1
seq_along(w)
# seq_len() creates a sequential vector in the length which is its input
# different to seq_along() in the sence of its input
# input to seq_len() is a scalar
1:n
seq(from = 1, to = n, by = 1)
x <- 1:n
y <- letters[1:n]
y2 <- LETTERS[1:n]
z <- runif(n)

# Indexing:

seq_along(w)
LETTERS[seq_along(w)]
letters[seq_along(w)]

# To add names to elements of a vector
names(w) <- letters[seq_along(w)]
w<0

# Input to 'which' is a logical expression
# which selects elements of a vector regarding the logical expression and just shows their indices.
which(w<0)
w[which(w<0)]
w[w<0]
w[-w<0]
# '-' changes the logical expression to its contradictory

# Now I want to select elements with distances in '2' in vector
seq(from = 1, to = 8, by = 2)
w[seq(from = 1, to = 8, by = 2)]
w[-seq(from = 1, to = 8, by = 2)]
w[c(2, 5)]
w[-c(2,5)]
w[c("a", "d", "f")]
# '-' doesn't work on charcteristic vectors
w[-c("a", "d", "f")]


# Lists:
# List is a vector with elements of different flavour or vectors of same or different flavours in
# in different lengths (if the flavours are not the same, lengths can be either the same or not.
# However, in the case that flavours are the same the lengths should be inequal to be a list,
# otherwise, it would be a dataframe)
# Dataframes are special cases of lists with vectors of same flavours and length
a <- list("Cabbage", pi, TRUE, 4.3)
str(c("Cabbage", pi, TRUE, 4.3))
# While creating vector, characters are of the forst priority, numeric and logical variables 
# come as second and next
str(a)
str(a[1])
str(a[[1]])
str(a[[1]][[1]])
# assigning a name to elements of a list
names(a) <- c("veg", "dessert", "myAim", "number")
a
a$veg
str(a$veg)
str(a$dessert[[1]])

# Another way to assign a name to elements of a list
b <- list(veg = "cabbage", dessert = pi, myAim = TRUE, number = 4.3)
names(b)
names(a)

# Indexing a list
a <- list(veg = c("cabbage", "eggplant"),
					tNum = c(pi, exp(1),
									 sqrt(2)),
					myAim = TRUE, joeNum = 2.6)



str(a)
mode(a)
class(a)
# in a list, both mode and class are a "list"
str(a$veg)
str(a$veg[[1]])

# This method of indexing is useful just in indexing of one elements
# if indexing of two elements is desireable, this method is not useful anymore
a[["joeNum"]]
a[[c("joeNum", "veg")]]

# To get two or more elements in alist, it is required to use single brackets
# Attention: The output is certainly a list
a[c("joeNum", "veg")]


# data.frames:
set.seed(1)
jDat0 <- data.frame(w=round(rnorm(n),2),
					 x = 1:n,
					 y = LETTERS[1:n])

str(jDat0)
# We see that component 'y' of the data frame is converted to "factor"
# To prevent that, we need to use I().
jDat <- data.frame(w = round(rnorm(n),2),
									 x = 1:n,
									 y = I(LETTERS[1:n]),
									 z = runif(n),
									 v = rep(LETTERS[9:12], each =2))
str(jDat)
# mode of the data frame is still a list
# Class of data.frame is "data.frame"
mode(jDat)
class(jDat)

jDat[[5]]
jDat$v
jDat[c("x", "z")]
subset(jDat, select = c("x", "z"))
# Using 'tidyverse::data_frame()' we can protect character vectors from conversion to factors.
library(tidyverse)
jDat1 <- data_frame(w=round(rnorm(n),2),
										x = 1:n,
										y = LETTERS[1:n])
str(jDat1)
mode(jDat1)
class(jDat1)

# How to convert a list to a data.frame?
# 'as.data.frame()' is useful in this sence
# In order to work, length of variables should be the same

jDat3 <- list(w=round(rnorm(n),2),
										x = 1:n,
										y = LETTERS[1:n])
str(jDat3)
as.data.frame(jDat3)
str(as.data.frame(jDat3)) #Character is converted to factor


(qDat <- list(w = round(rnorm(n), 2),
							x = 1:(n-1), ## <-- LOOK HERE! I MADE THIS VECTOR SHORTER!
							y = I(LETTERS[1:n])))
as.data.frame(qDat) ## does not work! elements don't have same length!

qDat$x <- 1:n
as.data.frame(qDat)

# 'outer(x, y, FUN)' fills the matrix. it repeats x using rep(x, each = length(x)) and 
# repeats y using rep(y, length(y)). Then, applies FUN and fills the matrix by stacking
#  the amounts in rows
(jmat <- outer(as.character(1:4), as.character(1:4),
			FUN = function(x,y){
				paste0('x',x,y)
			})
)
outer((1:4), (1:3), FUN = "*")
rep(1:4, each = length(1:3))
rep(1:3, length(1:4))
rep(1:4, each = length(1:3)) * rep(1:3, length(1:4))
mat <- outer((1:4), (1:3))

jmat2 <- outer(1:4, 1:3,
			FUN = function(x,y){
				paste0('x', x, y)
			})
str(jmat)
class(jmat)
mode(jmat)
dim(jmat)
length(jmat)		# Shows number of elements
nrow(jmat)
ncol(jmat)
rownames(jmat)
colnames(jmat)
rownames(jmat) <- paste0("row", seq_len(nrow(jmat)))
colnames(jmat) <- paste0("col", seq_len(ncol(jmat)))
jmat
dimnames(jmat)
dimnames(jmat)[[1]] <- paste0("Row", seq_len(nrow(jmat)))
dimnames(jmat)[[2]] <- paste0("Col", seq_len(ncol(jmat)))

dimnames(jmat) <- list(paste0("row", seq_len(nrow(jmat))),
											 paste0("col", seq_len(ncol(jmat))))
mode(mat)
mode(jmat2)
dim(jmat2)
# In a matrix, class is a "matrix", mode is the nature of constituents
vec <- c(1:4)
str(vec)
class(vec)
mode(vec)

# condense:
# 1)List: Both mode and class are list
# 2)data.frame: mode is list, class is data.frame
# 3)matrix: mode is the type of constiuents, class is matrix

# Indexing Matrixes:
jmat[2,3]
jmat[2,]
mode(jmat[2,])
class(jmat[2,])
is.vector(jmat[2,])
(mat0 <- jmat[,3, drop = FALSE])
#'drop = FALSE', to have just one of the rows or columns as matrix, not vector
class(mat0)
mode(mat0)
is.vector(mat0)
jmat[c("Row1", "Row4"), c("Col2", "Col3")]
jmat[-c(2,5), c(TRUE,TRUE, FALSE, FALSE)]

# "-" does not work for a character vector
jmat[-c("Row1", "Row4"), c("Col2", "Col3")]

# R is a column major order, meaning that it stacks column vectors to construct matrices
jmat[7] #The priority of column order is obvious here

grepl(c("2","4"), colnames(jmat))
grepl("[124]", colnames(jmat))
grep("[124]", colnames(jmat))
# grep(pattern, x), finds elements in x containing elements in pattern
# pattern should be in the "[]" shape. This converts pattern to a character vector
# if pattern is defined as a vector, then grep just considers the first item

# grep() the indices in 'x' matching with pattern
# grepl() gives a logical vector "TRUE" for elements matchiong with pattern, "FALSE"
# for elements not matching with pattern

jmat[1, grepl("[24]", colnames(jmat))]
jmat[1, c(2, 4)]
identical(jmat[1, grepl("[24]", colnames(jmat))], jmat[1, c(2, 4)])
# Assigning other values to matrix is possible by regular assignment operations after defining
# the matrix
jmat["Row1", c(2,3)] <- c("Hey", "This is nuts")

# Three ways to create matrixes:
# 	1) filling the matrix with a vector
# 	2) binding vectors together
# 	3) conversion of a DataFrame

# 1) filling the matrix with vectors
matrix(1:15, nrow =5)
matrix(1:15, nrow =6)
matrix(1:15, nrow =3)
matrix(1:15, nrow = 3, byrow = TRUE)
matrix(1:15, nrow = 5, byrow = TRUE)
matrix("yo", nrow = 5, ncol = 3)
matrix(c("yo", "hello"), nrow = 5, ncol = 4)

a <- c(1, 10, 50)
b <- c(20, 11, 31)
rbind(a, b)
cbind(a, b)

DF <- data.frame(vec1 = c(1, 10, 50), vec2 = c(20, 11, 31))
as.matrix(DF)
# Here, elements are not scilently converted to character since both vectors are of the same flavours
# But lets try vectors with different flavours
DF2 <- data.frame(vec1 = c(1, 10 , 13), vec2 = I(c("pi", "yo", "hello")))
str(DF2)

M2 <- as.matrix(DF2)
str(M2)
mode(M2)
class(M2)
# Putting it all together … implications for data.frames
jDat
# 1) Indexing a dat.frame like lists
jDat$z
str(jDat$z)
str(jDat[["z"]])

# 2) Vector style indexing of a data.frame
jDat["z"]
str(jDat["z"])
# It is still a dat.frame, but with just one variable
jDat[c("w", "z")]
subset(jDat, select = c("w", "z"))
str(subset(jDat, select = c("w", "z")))
str(jDat[c("w", "z")])
str(subset(jDat, select = c("w", "z")))

# 3) Matrix_Style Indexing
jDat[ , "v"]
str(jDat[ , "v"]) # It is a vector; to maintain it as a data.frame we have to ...
jDat[, "v", drop = FALSE]
str(jDat[, "v", drop = FALSE])
# 'subset(DF, subset = "v")', v should be a logical vector according to which the subset is made
jDat <- mutate(jDat, log = c(TRUE,TRUE, FALSE,TRUE, FALSE, FALSE, TRUE, FALSE))
subset(jDat, subset = log)
