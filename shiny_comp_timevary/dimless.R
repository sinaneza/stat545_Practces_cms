rm(E_g)
(E_g <- Exposed %>% 
	arrange(Time) %>% 
	group_by(Material, Coating, Cure, Time) %>% 
	mutate(R_mean = mean(Roughness, na.rm = TRUE),
				 R_median = median(Roughness, na.rm = TRUE),
				 H_mean = mean(Hardness, na.rm = TRUE),
				 H_median = median(Hardness, na.rm = TRUE),
				 F_mean = mean(Max_Flexural_Stress, na.rm = TRUE),
				 F_median = median(Max_Flexural_Stress, na.rm = TRUE)) %>% 
	ungroup() %>% 
	group_by(Material,  Coating, Cure) %>% 
		arrange(Time) %>% 
	mutate(GainedR_mean = (R_mean - first(R_mean))/first(R_mean),
				 GainedR_median = (R_median - first(R_median))/first(R_median),
				 NR_mean = (R_mean - min(R_mean, na.rm = TRUE))/
				 	(max(R_mean, na.rm = TRUE) - min(R_mean, na.rm = TRUE)),
				 NR_median = (R_median - min(R_median, na.rm = TRUE))/
				 	(max(R_median, na.rm = TRUE) - min(R_median, na.rm = TRUE)),
				 GainedNR_mean = (NR_mean - first(NR_mean))/first(NR_mean),
				 GainedNR_median = (NR_median - first(NR_median))/first(NR_median),
				 
				 GainedH_mean = (H_mean - first(H_mean))/first(H_mean),
				 LostH_median = (H_median - first(H_median))/first(H_median),
				 NH_mean = (H_mean - min(H_mean, na.rm = TRUE))/
				 	(max(H_mean, na.rm = TRUE) - min(H_mean, na.rm = TRUE)),
				 NH_median = (H_median - min(H_median, na.rm = TRUE))/
				 	(max(H_median, na.rm = TRUE) - min(H_median, na.rm = TRUE)),
				 GainedNH_mean = (NH_mean - first(NH_mean))/first(NH_mean),
				 GainedNH_median = (NH_median - first(NH_median))/first(NH_median),
				 
				 GainedF_mean = (F_mean - first(F_mean))/first(F_mean),
				 LostF_median = (F_median - first(F_median))/first(F_median),
				 NF_mean = (F_mean - min(F_mean, na.rm = TRUE))/
				 	(max(F_mean, na.rm = TRUE) - min(F_mean, na.rm = TRUE)),
				 NF_median = (F_median - min(F_median, na.rm = TRUE))/
				 	(max(F_median, na.rm = TRUE) - min(F_median, na.rm = TRUE)),
				 GainedNF_mean = (NF_mean - first(NF_mean))/first(NF_mean),
				 GainedNF_median = (NF_median - first(NF_median))/first(NF_median)) %>% 
	ungroup())

View(select (E_g,
						 Time, Material, Coating, Cure,
						 R_mean, NR_mean,
						 H_mean, NH_mean, GainedNH_mean,
						 F_mean, NF_mean))

Exposed %>% 
	group_by(Time,Material, Coating, Cure) %>% 
	mutate(H_mean = mean(Hardness, na.rm = TRUE)) %>% 
	ungroup() %>% 
	group_by(Material, Coating, Cure) %>% 
	mutate(MinH_mean = min(H_mean, na.rm = TRUE),
				 MaxH_mean = max(H_mean, na.rm = TRUE),
				 NormalH_mean = 
				 	(H_mean - min(H_mean,na.rm = TRUE))/
				 	(max(H_mean, na.rm = TRUE) - min(H_mean, na.rm = TRUE))) %>%
	ungroup() %>% 
	select(Time,Material, Coating, Cure,
				 Hardness, H_mean, MinH_mean,
				 MaxH_mean, NormalH_mean) %>% 
	arrange(Time,Material, Coating, Cure)

E_g %>% 
	select(Time, Material, Coating, Cure,
				 R_mean, GainedR_mean,
				 R_median, GainedR_median,
				 H_mean, LostH_mean,
				 H_median, LostH_median,
				 F_mean, LostF_mean,
				 F_median, LostF_median) %>%
	View()
	



E_g %>% 
	# select(Time, Material, Coating, Cure,
	# 			 R_mean, GainedR_mean,
	# 			 R_median, GainedR_median,
	# 			 H_mean, LostH_mean,
	# 			 H_median, LostH_median,
	# 			 F_mean, LostF_mean,
	# 			 F_median, LostF_median) %>% 
	# unique() %>%
	# filter(Material == "C") %>% 
	# View()
	# group_by(Material, Coating, Cure) %>%
	# summarize() %>% 
	# View()
	ggplot(aes(x = as_factor(as.character(Time)), y = GainedR_mean, colour = Material)) + 
	# geom_point(alpha = 1/3, size = 3) +
	stat_summary(fun.y = mean, geom = "line", aes(group = Material))
	# facet_wrap(~ Coating)


E_g %>% 
	ggplot(aes(x = as_factor(as.character(Time)),
						 y = NH_mean,
						 colour = Material)) +
	stat_summary(fun.y = mean, geom = "line", aes(group = Material)) +
	facet_wrap(~ Coating)


E_g %>% 
	ggplot(aes(x = as_factor(as.character(Time)), y = GainedNH_mean, colour = Material)) +
	stat_summary(fun.y = mean, geom = "line", aes(group = Material)) +
	facet_wrap( ~ Coating)


