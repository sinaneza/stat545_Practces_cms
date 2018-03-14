
(Exposed$Time)
E0 <- Exposed %>% 
	mutate(crit_4= str_c(Material, Coating, Cure, sep = "_")) 
E <- select(E0,Time, Hardness, Roughness) %>% 
	filter(!is.na(Hardness))
(Exposed.pca <- prcomp(E, center = TRUE,
				 scale = TRUE))

(types <- E0 %>% 
		filter(!is.na(Hardness)) %>%
	.$Material)
library(ggbiplot)
(g <- ggbiplot(Exposed.pca, obs.scale = 1, var.scale = 1, 
							 groups = types, ellipse = TRUE, 
							 circle = TRUE))
# g + facet_wrap(~ filter(Exposed, !is.na(Hardness))$Material)


data(iris)
head(iris, 3)
(log.ir <- log(iris[, 1:4]))
(ir.species <- iris[, 5])

(ir.pca <- prcomp(log.ir,
								 center = TRUE,
								 scale. = TRUE))

print(ir.pca)
summary(ir.pca)

predict(ir.pca, 
				newdata=tail(log.ir, 2))


library(devtools)
# install_github("ggbiplot", "vqv")

library(ggbiplot)
(g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
							groups = ir.species, ellipse = TRUE, 
							circle = TRUE))
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
							 legend.position = 'top')
print(g)
