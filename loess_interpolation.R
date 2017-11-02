rm(list = ls())
library("tidyverse")
mtcars
as_tibble(mtcars)

ggplot(mtcars, aes(x = hp, y = wt)) + geom_smooth(se = TRUE)
model <- loess(wt ~ hp, data=mtcars)
model2 <- lm(wt ~ splines::bs(hp, 3), data = mtcars)
model3 <- lm(wt ~ poly(hp, 3), data = mtcars)
model4 <- loess(wt ~ hp, data = mtcars, surface = "direct")  #Data is just predictable for loess just in this case.

xrange <- range(mtcars$hp)
xseq <- seq(from=xrange[1], to=xrange[2], length=100)
pred <- predict(model, newdata = data.frame(hp = xseq), se=TRUE)
y <-  pred$fit
ci <- pred$se.fit * qt(0.95 / 2 + .5, pred$df)
ymin = y - ci
ymax = y + ci
loess.DF <- data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)

ggplot(mtcars, aes(x=hp, y=wt)) +
	geom_point() +
	geom_line(aes(x = x, y= y), stat = "identity", data = select(loess.DF, c(x, y)))
	# geom_smooth(aes_auto(loess.DF), data=loess.DF, stat="identity")

ggplot(mtcars, aes(x=hp, y=wt)) +
	geom_point() +
	geom_smooth()

xseq2 <- seq(from = xrange[2], to = xrange[2]+300, length = 80)

pred2 <- predict(model, newdata2 = data.frame(hp = xseq2), se = TRUE)
y2 <- pred2$fit

y2
loess.DF2 <- data.frame(x = xseq2, y = y2)

XSeq_DF <- data_frame(hp = xseq)
XSeq2_DF <- data_frame(hp = xseq2)
library(modelr)
add_predictions(XSeq_DF, model)
add_predictions(XSeq2_DF, model)

add_predictions(XSeq_DF, model2)
add_predictions(XSeq2_DF, model2)


#Interpolation and Extrapolation by Linear Model(lm())
A <- add_predictions(XSeq_DF, model3)
add_predictions(XSeq2_DF, model3)

predict(model3, XSeq2_DF)

X_DF <- data_frame(hp = seq(from = xrange[1], to = xrange[2]+300, length = 200)) %>% 
	add_predictions(model3)
filter(X_DF, hp>=335)
ggplot(data = mtcars, aes(x = hp, y = wt)) + 
	geom_point() +
	geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
	geom_line(data = A,
						aes(x = hp, y = pred),
						colour = "red") +
	geom_line(data = filter(X_DF, hp < 400),
						aes(x = hp, y = pred),
						colour = "green") 

#Interpolation and Extrapolation by Local Regression (loess())
A2 <- add_predictions(XSeq_DF, model4)
add_predictions(XSeq2_DF, model4)
X_DF2 <- data_frame(hp = seq(from = xrange[1], to = xrange[2]+300, length = 200)) %>% 
	add_predictions(model4)

ggplot(data = mtcars, aes(x = hp, y = wt)) + 
	geom_point() +
	geom_smooth () +
	geom_line(data = A2,
 						aes(x = hp, y = pred),
 						colour = "red") +
	geom_line(data = filter(X_DF2, hp < 400),
						aes(x = hp, y = pred),
						colour = "green")



Model <- lm(wt ~ hp, data = mtcars)
library(broom)
tidy(Model3)
augment(Model3)
-0.6021063*(mtcars$hp[[1]])^3 - 2.0643041*(mtcars$hp[[1]])^2 + 3.5887402*(mtcars$hp[[1]]) + 3.2172500
0.00940096*mtcars$hp[[1]] + 1.83824670
mtcars$wt[[1]]

Model1 <- lm(wt ~ poly(hp), data = mtcars)
tidy(Model1)
poly(mtcars$hp)[1]

3.58874 * mtcars$hp[[1]] + 3.21725
3.58874 * poly(mtcars$hp)[1] + 3.21725

Model2 <- lm(wt ~ poly(hp,2), data = mtcars)
(T2 <- tidy(Model2))
augment(Model2)[1, ".fitted"]
(a1 <- T2[[3, "estimate"]]*poly(mtcars$hp,2)[[1,2]] + T2[[2, "estimate"]]*poly(mtcars$hp,2)[[1,1]] + T2[[1, "estimate"]])
# (b1 <- T2[[3, "estimate"]]*poly(mtcars$hp,2)[[1,2]]^2 + T2[[2, "estimate"]]*poly(mtcars$hp,2)[[1,2]] + T2[[1, "estimate"]])
abs(a1-augment(Model2)[1, ".fitted"])
# abs(b1-augment(Model2)[1, ".fitted"])

augment(Model2)[2, ".fitted"]
(a2 <- T2[[3, "estimate"]]*poly(mtcars$hp,2)[[2,2]] + T2[[2, "estimate"]]*poly(mtcars$hp,2)[[2,1]] + T2[[1, "estimate"]])
# (b2 <- -2.064304*poly(mtcars$hp,2)[[2,2]] + 3.588740*poly(mtcars$hp,2)[[2,2]] + 3.217250)
abs(a2-augment(Model2)[2, ".fitted"])
# abs(b2-augment(Model2)[2, ".fitted"])

augment(Model2)[3, ".fitted"]
(a3 <- T2[[3, "estimate"]]*poly(mtcars$hp,2)[[3,2]] + T2[[2, "estimate"]]*poly(mtcars$hp,2)[[3,1]] + T2[[1, "estimate"]])
# (b3 <- -2.064304*poly(mtcars$hp,2)[[3,2]]^2 + 3.588740*poly(mtcars$hp,2)[[3,2]] + 3.217250)
abs(a3-augment(Model2)[3, ".fitted"])
# abs(b3-augment(Model2)[3, ".fitted"])





(Model3 <- lm(wt ~ poly(hp,3), data = mtcars))
(T3 <- tidy(Model3))
augment(Model3)[[1, ".fitted"]]
(aa1 <- T3[[4, "estimate"]]*poly(mtcars$hp,3)[[1,3]] + T3[[3, "estimate"]]*poly(mtcars$hp,3)[[1,2]] + 
		T3[[2, "estimate"]]*poly(mtcars$hp,3)[[1,1]] + T3[[1, "estimate"]])
abs(aa1-augment(Model3)[[1, ".fitted"]])