rm(list = ls())
library(tidyverse)
library(stringr)
library(forcats)
library(purrr)
library(listviewer)
library(readr)
library(readxl)

(A <- read_excel("C:/additional/UBC/MENG_Papers/Weather_Kelowna/Weather_Canada/Kelowna_2014_2.xlsx"))
(B <- read_excel("C:/additional/UBC/MENG_Papers/Weather_Kelowna/Weather_Canada/Kelowna_2015.xlsx"))

(UBCO_2014 <- read_excel("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/UBCO_2014.xlsx"))
(UBCO_2015 <- read_excel("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/UBCO_2015.xlsx"))
(UV <- read_excel("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/UV_WeatherCom.xlsx"))
names(UBCO_2015) <- names(UBCO_2014)
names(B) <- names(A)

(UBCO_2014 <- UBCO_2014 %>% 
	replace_na(list(Snow_on_Grnd_cm = 0)))

(UBCO_2015 <- UBCO_2015 %>% 
		replace_na(list(Snow_on_Grnd_cm = 0)))

UBCO_2014 %>% 
	View

# tibble(A= names(UBCO_2014), B = names(UBCO_2015)) %>% 
# 	View()

# UBCO Weather Station Data:
(UBCO <- bind_rows(UBCO_2014, UBCO_2015) %>% 
		filter(!is.na(Time_char))) %>% 
	View()


(Weather_UBCO_Time0 <- tibble(Time = 0, month = "Jan", precipitation = 0,
												 mean_temp = -7.5, min_temp =-9.5, max_temp = -5.4, SnowLevel_cm = 0))

(Weather_UBCO_1 <- UBCO %>% 
		group_by(Time_char) %>%
		summarize(precipitation = sum(Total_Precip_mm, na.rm = TRUE),
							mean_temp = mean(Mean_Temp_C, na.rm = TRUE),
							min_temp = mean(Min_Temp_C, na.rm = TRUE),
							max_temp = mean(Max_Temp_C, na.rm = TRUE),
							SnowLevel_cm = mean(Snow_on_Grnd_cm, na.rm = TRUE)) %>% 
		ungroup() %>% 
		mutate(month = c("Feb_March", "March_May", "May_Aug", "Aug_Nov", "Nov_Feb")) %>% 
		mutate(Time = c(30, 90, 180, 270, 360)) %>% 
		select(Time, month, precipitation,
					 mean_temp, min_temp, max_temp, SnowLevel_cm))
UBCO %>% 
	filter(270<=Time & Time <360) %>% 
	with(sum(Total_Precip_mm, na.rm =TRUE))

(Weather_UBCO <- bind_rows(Weather_UBCO_Time0, Weather_UBCO_1))

(Weather_UV <- UV %>%
		group_by(Time) %>% 
		summarize(mean_UV = mean(Mean_UV_Index)))

(Weather_UBCO_UV <- inner_join(Weather_UBCO, Weather_UV))
saveRDS(Weather_UBCO_UV, "Weather_UBCO_UV.rds")
write_csv(Weather_UBCO_UV, "C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/Weather_UBCO_UV.csv")
UBCO %>%
	filter(Time>=90 & Time<180) %>% 
	with(sum(Total_Precip_mm,na.rm = TRUE))

(UV_plot <- Weather_UBCO_UV %>% 
	ggplot(aes(x = Time, y= mean_UV)) + 
		scale_x_continuous("Days", breaks = c(0,90,180,270,360)) +
	geom_point(size =3) + 
	labs(x = "Days", y = "Average UV Index") +
	geom_smooth(data = UV, aes(x = Time, y = Mean_UV_Index, group = 1),
							se = FALSE,
							colour = "blue",
						linetype="longdash") +
	theme(plot.subtitle = element_text(vjust = 1),
				plot.caption = element_text(vjust = 1),
				panel.grid.major = element_line(colour = "gray5",
																				linetype = "longdash"),
				panel.grid.minor = element_line(colour = "gray5",
																				linetype = "dotdash"),
				panel.background = element_rect(fill = "gray100"),
				axis.text = element_text(colour = "gray5")))


ggsave("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/UV.png", UV_plot, scale = 1)


(precipitation_plot <- Weather_UBCO_UV %>% 
	ggplot(aes(x= Time, y = precipitation)) +
	scale_x_continuous("Days", breaks = c(0, 90, 180, 270, 360)) +
	labs( y = "Precipitation in mm") +
	geom_point(size = 3) +
	geom_smooth(se = FALSE, linetype = "longdash") +
	theme(plot.subtitle = element_text(vjust = 1),
				plot.caption = element_text(vjust = 1),
				panel.grid.major = element_line(colour = "gray5",
																				linetype = "longdash"),
				panel.grid.minor = element_line(colour = "gray5",
																				linetype = "dotdash"),
				panel.background = element_rect(fill = "gray100"),
				axis.text = element_text(colour = "gray5")))

ggsave("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/Precipitation.png", precipitation_plot, scale = 1)

(Temp_plot <- Weather_UBCO_UV %>% 
		ggplot(aes(x= Time, y = mean_temp)) +
		scale_x_continuous("Days", breaks = c(0, 90, 180, 270, 360)) +
		labs( y = "Temperature in C") +
		geom_point(size = 3) +
		geom_smooth(se = FALSE, linetype = "longdash") +
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")))


ggsave("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/Temperature.png", Temp_plot, scale = 1)

(Snow_plot <- Weather_UBCO_UV %>% 
		ggplot(aes(x= Time, y = SnowLevel_cm)) +
		scale_x_continuous("Days", breaks = c(0, 90, 180, 270, 360)) +
		labs( y = "Snow Level in cm") +
		geom_point(size = 3) +
		geom_smooth(se = FALSE, linetype = "longdash") +
		theme(plot.subtitle = element_text(vjust = 1),
					plot.caption = element_text(vjust = 1),
					panel.grid.major = element_line(colour = "gray5",
																					linetype = "longdash"),
					panel.grid.minor = element_line(colour = "gray5",
																					linetype = "dotdash"),
					panel.background = element_rect(fill = "gray100"),
					axis.text = element_text(colour = "gray5")))

ggsave("C:/additional/UBC/MENG_Papers/Weather_Kelowna/UBCO/Snow.png", Snow_plot, scale = 1)

# Different Weather Station Data
A_B <- bind_rows(A,B) %>% 
	filter(!is.na(Time_char))

A_B %>%
	View()

(Weather0 <- A_B %>% 
	group_by(Time_char) %>% 
	summarize(precipitation = sum(Total_Precip_mm, na.rm = TRUE),
						mean_temp = mean(Mean_Temp_C, na.rm = TRUE),
						min_temp = mean(Min_Temp_C, na.rm = TRUE), 
						max_temp = mean(Max_Temp_C, na.rm = TRUE)) %>% 
	mutate(Time = c(30, 90, 180, 270, 360)) %>% 
	select(Time, Time_char, precipitation, mean_temp, min_temp, max_temp))

A_B %>% 
	filter((Time>30)&(Time<=90)) %>% 
	with(sum(Total_Precip_mm, na.rm = TRUE))

C <- tibble(Time = 0, Time_char = "A0",
						max_temp = 25, min_temp = 25, mean_temp = 25,
						precipitation = 0)
bind_rows(C,Weather0)
