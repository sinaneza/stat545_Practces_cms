rm(list = ls())
library(tidyverse)
library(readxl)
library(forcats)
library(stringr)

exposed30 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												sheet = 1)

exposed90 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												sheet = 2)

exposed180 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 3)

exposed270 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 4)

exposed360 <- read_excel("C:/additional/UBC/MENG_Papers/Bryn/Generated_Data/Exposed_30.xlsx",
												 sheet = 5)

(Exposed_list <- list(exposed30 = exposed30, exposed90 = exposed90, exposed180 = exposed180,
		 exposed270 = exposed270, exposed360 = exposed360))


(Exposed <- bind_rows(bind_rows(Exposed_list$exposed30, Exposed_list$exposed90,
																Exposed_list$exposed180, Exposed_list$exposed270, Exposed_list$exposed360)) %>% 
		mutate(Cure = as_factor(Cure), Material = as_factor(Material),
					 Coating = as_factor(Coating))
	)
Exposed %>%
	mutate(Time = as_factor(as.character (Time)))
## 1) Separate Material, Coating, Cure
# 1-a) Roughness

# Chopped roughness gets more distributed by the pass of time.
# The correlation of other two variables (coting and cure) is more important after the day 180.
# (Especially for chopped reinforced materials)

# Roughness scale has mainly an increasing trend by the pass of time

# Seems the trend of roughness scale is mainly a function of resin, while the values change 
#by the usage of reinforcement

# Usage of reinforcement causes an increse in roughness, especially after the day 180,compared to 
# pure resin samples.

# 1-a-1)Roughness, Material
(plot_mateial_rough <- Exposed %>%
	filter(!is.na(Roughness)) %>% 
	ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3) + 
		theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.grid.major = element_line(colour = "gray5", 
        linetype = "longdash"), panel.grid.minor = element_line(colour = "gray5", 
        linetype = "dotdash"), panel.background = element_rect(fill = "gray100")) + 
		theme(axis.text = element_text(colour = "gray5")))

(plot_mateial_rough_box <- Exposed %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material)) +
		geom_boxplot()) + stat_summary(fun.y = mean, geom = "point", size = 2)

# 1-a-2) Roughness Coating
(plot_coating_rough <- Exposed %>% 
	filter(!is.na(Roughness)) %>% 
	ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Coating)) +
	geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3)
)	
# samples with no applied Gelcoat get more distributed
# The correlation of other two variables is major after the day 180, especially for un-coted
# reinforced materials.

# The increase of hardness in un-applied Gel Coat samples is more intensive after the day 180
# There is a general increasing trend in roughness by the pass of time.
# However, the Gel Coat applied samples are mainly stable in roughness compared to samples 
# comprised purely from resin

# Seems that roughness is more correlated to coating rather than the material and cure.

(plot_coating_rough_box <- Exposed %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Coating)) +
		geom_boxplot() + stat_summary(fun.y = mean, geom = "point", size = 2)
)

# 1-a-3) Roughness, Coating
# Concluding cure does not have much effect on roughness
(plot_cure_rough <- Exposed %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Cure)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3)
)

(plot_cure_rough_box <- Exposed %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Cure)) +
		geom_boxplot() +  stat_summary(fun.y = mean, geom = "point", size = 2)
)

# Ranking of samples regarding their material is:
# 1) Pure Resin 2) Chopped 3) Woven (Ranked in increasing trend)

# Distribution of data is more for reinforced samples (copped and woven) cmpared to
# samples purely made up of resin

# Generel trend is increasing  till 180 days.
# Then, hardness scale experiences a slight decrease at day 270 and then stability at day 360.

# Seems that the trend of hardness scale is mainly related to the resin, while the usage of
# reinforcement just increases the values. Values for woven reinforced material is more than
# chopped ones.

# 1-b) Hardness
# 1-b-1) Hardness, Material
(plot_material_hard <- Exposed %>%
	filter(!is.na(Hardness)) %>% 
	ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Material)) +
	geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))


(plot_material_hard_box <- Exposed %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Material)) +
		geom_boxplot() +  stat_summary(fun.y = mean, geom = "point", size = 2))

# It is more visible by coating filgures for hardness that the trnd of changes in hardness 
# is defined by the resin

# Gel Coated samples have less hardness than non-coated ones, and both types have more hardness
# compared to oure resin samples

# Seems that usage of coating is more corelated to hardness rather than the type of material and cure.

# Trend is increasing for all samples (coated or un-coated) till the day 180. Then there is 
# a slight decrease after the day 270. After the day 270, hardness scale remains stable.

# The reinforced samples have more values in roughness rather than pure resin samples. All 

# un-coated samples have more hardness compared to coated samples.

# 1-b-2) Hardness Coating
(plot_coating_hard <- Exposed %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Coating)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))

(plot_coating_hard_box <- Exposed %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Coating)) +
		geom_boxplot() +  stat_summary(fun.y = mean, geom = "point", size = 2))

# Cure does not have much effect on hardness

# 1-b-3) Hardness Cure
(plot_cure_hard <- Exposed %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Cure)) +
		geom_jitter( position = position_jitter(width = 1/3, height = 0), alpha = 1/3, size = 3))

(plot_cure_hard_box <- Exposed %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Cure)) +
		geom_boxplot() +  stat_summary(fun.y = mean, geom = "point", size = 2))

##2) Material_Coating
(Exposed_collapsed1 <- Exposed %>% 
	mutate(Material_Coating = str_c(Material, Coating, sep= "_")))

# 2-1)Roughness


(plot_mateial_coating_rough <- Exposed_collapsed1 %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material_Coating)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))

(plot_mateial_coating_rough_box1 <- Exposed_collapsed1 %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material_Coating)) +
		geom_boxplot() + stat_summary(fun.y = mean, geom = "point", size = 2))

(plot_mateial_coating_rough_ave <- Exposed_collapsed1 %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material_Coating)) +
		stat_summary(fun.y = mean, geom = "point", size = 2) + geom_line(data = Exposed_collapsed1 %>% 
																																				filter(!is.na(Roughness)) %>% 
																																				group_by(Time, Material_Coating) %>% 
																																				summarise(mean = mean(Roughness)),
																																			position = "identity",
																																			stat = "identity",
																																			aes(x = as_factor(as.character (Time)),
																																					y = mean, group = Material_Coating)))

(plot_mateial_coating_rough_box2 <- Exposed_collapsed1 %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material_Coating)) +
		geom_boxplot() + stat_summary(fun.y = mean, geom = "point", size = 2)) +
	facet_wrap(~ Material_Coating)


## 2-2) Hardness 
(plot_mateial_coating_hard <- Exposed_collapsed1 %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Material_Coating)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))

(plot_mateial_coating_hard_box1 <- Exposed_collapsed1 %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Material_Coating)) +
		geom_boxplot() +stat_summary(fun.y = mean, geom = "point", size = 2)) 


(plot_mateial_coating_hard_box2 <- Exposed_collapsed1 %>%
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Hardness, colour = Material_Coating)) +
		geom_boxplot() + stat_summary(fun.y = mean, geom = "point", size = 2)) +facet_wrap(~Material_Coating)

(plot_mateial_coating_hard_ave <- Exposed_collapsed1 %>% 
	filter(!is.na(Hardness)) %>% 
	ggplot(aes(x = as_factor(as.character(Time)), y = Hardness, colour = Material_Coating)) + 
	scale_x_discrete("Time") +
	stat_summary(fun.y = mean, geom = "point", size = 2) + 
	geom_line(data = Exposed_collapsed1 %>% 
							filter(!is.na(Hardness)) %>% 
							group_by(Time, Material_Coating) %>% 
							summarise(mean = mean(Hardness)),
						aes(x = as_factor(as.character(Time)), y = mean,
								group = Material_Coating)))


# 3) Matrial_Coating_Cure

(Exposed_collapsed2 <- (Exposed_collapsed1 %>% 
	mutate(Material_Coating_Cure = str_c(Material_Coating, Cure, sep = "_"))))

# 3-1) Roughness
(plot_mateial_coating_cure_rough <- Exposed_collapsed2 %>% 
	filter((!is.na(Roughness))) %>% 
	ggplot(aes(x = as_factor(as.character(Time)), y = Roughness,
						 colour = Material_Coating_Cure)) +
		scale_x_discrete("Time")+
	geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))

(plot_material_coating_cure_rough_box1 <- Exposed_collapsed2 %>% 
		filter((!is.na(Roughness))) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Roughness,
							 colour = Material_Coating_Cure)) +
		scale_x_discrete("Time") +
		geom_boxplot() +
		stat_summary(fun.y = mean, geom = "point", size = 2) +
		facet_wrap(~ Material_Coating)
	)

(plot_material_coating_cure_rough_ave <- Exposed_collapsed2 %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Roughness,
							 colour = Material_Coating_Cure)) + 
		scale_x_discrete("Time") +
		stat_summary(fun.y = mean, geom = "point", size = 2) + 
		geom_line(data = Exposed_collapsed2 %>% 
								filter(!is.na(Roughness)) %>% 
								group_by(Time, Material_Coating_Cure) %>% 
								summarise(mean = mean(Roughness)),
							aes(x = as_factor(as.character(Time)), y = mean,
									group = Material_Coating_Cure)))

(plot_material_coating_cure_rough_ave2 <- Exposed_collapsed2 %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Roughness,
							 colour = Material_Coating_Cure)) + 
		scale_x_discrete("Time") +
		stat_summary(fun.y = mean, geom = "point", size = 2) + 
		geom_line(data = Exposed_collapsed2 %>% 
								filter(!is.na(Roughness)) %>% 
								group_by(Time,Material_Coating, Material_Coating_Cure) %>% 
								mutate (mean = mean(Roughness)),
							aes(x = as_factor(as.character(Time)), y = mean,
									group = Material_Coating_Cure))+ facet_wrap(~ Material_Coating))

(MCC_Ranked_Mean_Rough <- Exposed_collapsed2 %>% 
	filter(!is.na(Roughness)) %>% 
	group_by(Time, Material_Coating_Cure) %>% 
	summarize (Mean_rough = mean(Roughness)) %>% 
	ungroup() %>%
	group_by(Time) %>%
	mutate(ranked_rough = Material_Coating_Cure[min_rank(Mean_rough)]))
View(MCC_Ranked_Mean_Rough)
# 3-2) Hardness

(plot_mateial_coating_cure_hard <- Exposed_collapsed2 %>% 
		filter((!is.na(Hardness))) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
							 colour = Material_Coating_Cure)) +
		scale_x_discrete("Time")+
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))

(plot_material_coating_cure_hard_box1 <- Exposed_collapsed2 %>% 
		filter((!is.na(Hardness))) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
							 colour = Material_Coating_Cure)) +
		scale_x_discrete("Time") +
		geom_boxplot() +
		stat_summary(fun.y = mean, geom = "point", size = 2) +
		facet_wrap(~ Material_Coating)
)

(plot_MCC_hard_ave1 <- Exposed_collapsed2 %>% 
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
							 colour = Material_Coating_Cure)) + 
		scale_x_discrete("Time") +
		stat_summary(fun.y = mean, geom = "point", size = 2) + 
		geom_line(data = Exposed_collapsed2 %>% 
								filter(!is.na(Hardness)) %>% 
								group_by(Time, Material_Coating_Cure) %>% 
								summarise(mean = mean(Hardness)),
							aes(x = as_factor(as.character(Time)), y = mean,
									group = Material_Coating_Cure)))

(plot_MCC_hard_ave2 <- Exposed_collapsed2 %>% 
		filter(!is.na(Hardness)) %>% 
		ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
							 colour = Material_Coating_Cure)) + 
		scale_x_discrete("Time") +
		stat_summary(fun.y = mean, geom = "point", size = 2) + 
		geom_line(data = Exposed_collapsed2 %>% 
								filter(!is.na(Hardness)) %>% 
								group_by(Time,Material_Coating, Material_Coating_Cure) %>% 
								summarise(mean = mean(Hardness)),
							aes(x = as_factor(as.character(Time)), y = mean,
									group = Material_Coating_Cure))+ 
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")) + facet_wrap(~ Material_Coating))


Exposed_collapsed2 %>% 
	filter(!is.na(Hardness)) %>% 
	ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
						 colour = Material_Coating_Cure)) + 
	scale_x_discrete("Time") +
	stat_summary(fun.y = mean, geom = "point", size = 2) + 
	geom_line(data = Exposed_collapsed2 %>% 
							filter(!is.na(Hardness)) %>% 
							group_by(Time, Material_Coating_Cure) %>% 
							summarise(mean = mean(Hardness)),
						aes(x = as_factor(as.character(Time)), y = mean,
								group = Material_Coating_Cure)) + facet_wrap( ~ Material_Coating)

(MCC_Ranked_Mean_Hard <- Exposed_collapsed2 %>% 
		filter(!is.na(Hardness)) %>% 
		group_by(Time, Material_Coating_Cure) %>% 
		summarize (Mean_hard = mean(Hardness)) %>% 
		mutate(ranked_hard = Material_Coating_Cure[min_rank(Mean_hard)]))
View(MCC_Ranked_Mean_Hard)


# Material_Cure
(Exposed_Collapsed <- Exposed_collapsed2 %>% 
	mutate (Material_Cure = str_c(Material, Cure,sep= "_"),
					Coating_Cure = str_c(Coating, Cure,sep= "_")))

(plot_MCu_rough <- Exposed_collapsed %>%
		filter(!is.na(Roughness)) %>% 
		ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = Material_Cure)) +
		geom_jitter( position = position_jitter(width = 0.1, height = 0), alpha = 1/3, size = 3))


(vars0 <- c("Cure", "Coating"))
(vars1 <- c("Material", "Coating", "Cure"))
(vars <- vars1[vars1 %in% vars0])

(TP <- Exposed %>% 
	mutate(colour = str_c(Exposed[[vars[[1]]]], Exposed[[vars[[2]]]], sep = "_")) %>% 
	filter(!is.na(Roughness)) %>% 
	ggplot(aes( x = as_factor(as.character (Time)),
							y = Roughness, colour = colour)) +
	geom_jitter( position = position_jitter(width = 0.1, height = 0),
							 alpha = 1/3, size = 3))

dim(TP$data)

(TP2 <- TP + facet_wrap(~ TP$data[[vars1[!vars1 %in% vars0]]]))

(AP <- Exposed %>% 
		mutate(colour = str_c(Exposed[[vars[[1]]]], Exposed[[vars[[2]]]], sep = "_")) %>% 
		filter(!is.na(Roughness)) %>% 
		ggplot(aes( x = as_factor(as.character (Time)),
								y = Roughness, colour = colour)) +
		stat_summary(aes(group = colour),fun.y = mean, geom = "line", size = 2) 
		# stat_summary(fun.y = mean, geom = "line", group = colour)) 
	
