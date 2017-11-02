set.seed(123)           
frame <- data.frame(year = rep(1980:2000, 10), y = sample(1:1000, 210))
head(frame)

frame1 <- frame[frame$year %in% c(1980:1990),]
frame2 <- frame[frame$year %in% c(1980:2000),]
frame1
frame2
ggplot(frame2, aes(x= year , y = y))+
	geom_point() + 
	geom_smooth(lwd=3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 4)) +
	geom_smooth(data=frame1,
							method = glm, fullrange = TRUE
							lwd = 3, se = FALSE, colour = "red",)
