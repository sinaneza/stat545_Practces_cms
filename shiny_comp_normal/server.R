
server <- function(input, output, session) {
	library(tidyverse)
	library(stringr)
	library(forcats)
	
	
	
	Mutated_crit <- reactive({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			Mutate_VarCollapse_criteria(data = Exposed, collapsing_vars = input$AnalCrit)
		}
	})
	
	Mutated <- reactive({
		if(is.null(Mutated_crit())) {
			return(NULL)
		} else if(length(input$AnalCrit) < 3) {
			Mutate_VarCollapse_facet(data = Mutated_crit(),
															 collapsing_vars = crit_vars[!(crit_vars %in% input$AnalCrit)])
		} else {
			mutate(Mutated_crit(),
						 facet = str_c(Material, Coating,
						 							sep = "_"))
		}
	})
	
	# Mutated <- reactive({if (is.null(input$AnalCrit)) {
	# 	return (NULL)
	# } else {
	# 	Mutate_Vars(data = Exposed, vars = input$AnalCrit, crit_vars = crit_vars)
	# }
	# 	})
	
	
	colour_scheme <- reactive({
		if (is.null(input$AnalCrit)){
			return (NULL)
		}
		str_c(input$AnalCrit, collapse = "_")
	})
	
	Colour <- reactive({
		if (is.null(colour_scheme())){
			return(NULL)
		}
		unique(colour[[colour_scheme()]])
	})
	
	
	
	
	
	
	# Roughness, Scatter Plot:
	
	
	plot_sizeTab1 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab1),
												 as.numeric(input$PlotSizeTab1),
												 2 * as.numeric(input$PlotSizeTab1),
												 as.numeric(input$PlotSizeTab1))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_rough_jitter <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Roughness, colour = criteria)) +
				scale_x_discrete("Time") +
				geom_jitter(position = position_jitter(width = 0.1, height = 0),
										 alpha = 1/3, size = 3) +
				stat_summary(fun.y = "mean", geom = "line",
										 size = 1, alpha = 1/2,
										 linetype="longdash",
										 aes(group = criteria)) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))	
		}
		
	})
	
	# Roughness, Scatter Plot1:
	output$rough_jitter1_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else{
			plotOutput("rough_jitter1",
								 height = plot_sizeTab1()[[4]])
		}
	})
	
	
	output$rough_jitter1 <- renderPlot({
		plot_rough_jitter()
	})
	
	# Roughness, Scatter Plot2:
	
	
	output$rough_jitter2_UIO <- renderUI({
		# plotOutput("rough_jitter2",
		# 					 height = plot_sizeTab1()[[length(input$AnalCrit)]])
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_jitter2",
								 height = plot_sizeTab1()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$rough_jitter2 <- renderPlot({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plot_rough_jitter() +
				facet_wrap(~ facet)
		}
	})
	
	# # Scatter Plot, Varying Time:
	# 
	# plot2_sizeTab1 <- reactive({
	# 	str_c(as.character(c(2 * as.numeric(input$Plot2SizeTab1),
	# 											 as.numeric(input$Plot2SizeTab1),
	# 											 2 * as.numeric(input$Plot2SizeTab1),
	# 											 as.numeric(input$Plot2SizeTab1))),
	# 				c("px", "px", "px", "px"), sep = "")
	# })
	# 
	# filtered <- reactive({
	# 	if (is.null(Mutated())){
	# 		return(NULL)
	# 	}
	# 	Mutated() %>%
	# 		filter(Time2 == input$TimeRJ)
	# })
	# 
	# plot_rough_JT <- reactive({
	# 	if(is.null(filtered())){
	# 		return(NULL)
	# 	} else {
	# 		filtered() %>%
	# 			filter(!is.na(Roughness)) %>%
	# 			mutate(criteria = fct_drop(criteria)
	# 						 # criteria = fct_reorder(criteria, Roughness,
	# 						 # 												 fun = median)
	# 			) %>%
	# 			ggplot(aes(x = criteria, y = Roughness,
	# 								 colour = criteria)) + 
	# 			scale_x_discrete("criteria") +
	# 			scale_y_continuous("Roughness",
	# 												 limits = c(min(Mutated()$Roughness,
	# 												 							 na.rm = TRUE),max(Mutated()$Roughness,
	# 												 							 									na.rm = TRUE))) +
	# 			geom_jitter(position = position_jitter(width = 0.1, height = 0),
	# 									alpha = 1/3, size = 3) + 
	# 			scale_colour_manual(values = Colour()) +
	# 			theme(plot.subtitle = element_text(vjust = 1),
	# 						plot.caption = element_text(vjust = 1),
	# 						panel.grid.major = element_line(colour = "gray5",
	# 																						linetype = "longdash"),
	# 						panel.grid.minor = element_line(colour = "gray5",
	# 																						linetype = "dotdash"),
	# 						panel.background = element_rect(fill = "gray100"),
	# 						axis.text = element_text(colour = "gray5"))
	# 	}
	# 	
	# })
	# 
	# # Roughness, Time Varying Scatter Plot1:
	# 
	# output$rough_JT1_UIO <- renderUI({
	# 	if(is.null(input$AnalCrit)){
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("rough_JT1",
	# 							 height = plot2_sizeTab1()[[4]])
	# 	}
	# })
	# output$rough_JT1 <- renderPlot({
	# 	plot_rough_JT()
	# })
	# 
	# 
	# # Roughness, Time Varying Scatter Plot2:
	# output$rough_JT2_UIO <- renderUI({
	# 	if (is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("rough_JT2",
	# 							 height = plot2_sizeTab1()[[length(input$AnalCrit)]])
	# 	}
	# })
	# 
	# output$rough_JT2 <- renderPlot({
	# 	if(is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plot_rough_JT() + facet_wrap(~ facet)
	# 	}
	# 	
	# })
	
	# Box Plot1:
	
	plot_sizeTab2 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab2),
												 as.numeric(input$PlotSizeTab2),
												 2 * as.numeric(input$PlotSizeTab2),
												 as.numeric(input$PlotSizeTab2))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_rough_box <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>% 
				filter(!is.na(Roughness)) %>% 
				ggplot(aes(x = as_factor(as.character(Time)), y = Roughness, colour = criteria)) +
				scale_x_discrete("Time") +
				geom_boxplot(size = 0.75) + 
				stat_summary(fun.y = mean, geom = "point", size = 2) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	output$rough_box1_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_box1", height = plot_sizeTab2()[[4]])
		}
	})
	# if (length(input$AnalCrit) < 3)
	
	
	output$rough_box1 <- renderPlot({
		plot_rough_box()
		
		# if ((is.null(input$AnalCrit)) | (length(input$AnalCrit) == 3)) {
		# 	return(NULL)
		# } else {
		# 	plot_rough_box()
		# }
	})
	
	# Box Plot2:
	
	output$rough_box2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_box2", 
								 height = plot_sizeTab2()[[length(input$AnalCrit)]])
		}
	})
	
	output$rough_box2 <- renderPlot({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plot_rough_box() +
				facet_wrap( ~ facet)
		}
	})
	
	# # Box Plot, Varying Time:
	# 
	# plot2_sizeTab2 <- reactive({
	# 	str_c(as.character(c(2 * as.numeric(input$Plot2SizeTab2),
	# 											 as.numeric(input$Plot2SizeTab2),
	# 											 2 * as.numeric(input$Plot2SizeTab2),
	# 											 as.numeric(input$Plot2SizeTab2))),
	# 				c("px", "px", "px", "px"), sep = "")
	# })
	# 
	# filtered_BT <- reactive({
	# 	if (is.null(Mutated())){
	# 		return(NULL)
	# 	}
	# 	Mutated() %>%
	# 		filter(Time2 == input$TimeRB)
	# })
	# 
	# plot_rough_BT <- reactive({
	# 	if(is.null(filtered_BT())){
	# 		return(NULL)
	# 	} else {
	# 		filtered_BT() %>%
	# 			filter(!is.na(Roughness)) %>%
	# 			mutate(criteria = fct_drop(criteria)
	# 						 # criteria = fct_reorder(criteria, Roughness,
	# 						 # 												 fun = median)
	# 			) %>%
	# 			ggplot(aes(x = criteria, y = Roughness,
	# 								 colour = criteria)) +
	# 			scale_x_discrete("criteria") +
	# 			scale_y_continuous("Roughness",
	# 												 limits = c(min(Mutated()$Roughness,
	# 												 							 na.rm = TRUE),
	# 												 					 max(Mutated()$Roughness,
	# 												 							 									na.rm = TRUE))) +
	# 			geom_boxplot() +
	# 			scale_colour_manual(values = Colour()) +
	# 			theme(plot.subtitle = element_text(vjust = 1),
	# 						plot.caption = element_text(vjust = 1),
	# 						panel.grid.major = element_line(colour = "gray5",
	# 																						linetype = "longdash"),
	# 						panel.grid.minor = element_line(colour = "gray5",
	# 																						linetype = "dotdash"),
	# 						panel.background = element_rect(fill = "gray100"),
	# 						axis.text = element_text(colour = "gray5"))
	# 	}
	# 	
	# })
	
	# # Roughness, Time Varying Box Plot1:
	# 
	# output$rough_BT1_UIO <- renderUI({
	# 	if(is.null(input$AnalCrit)){
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("rough_BT1",
	# 							 height = plot2_sizeTab2()[[4]])
	# 	}
	# })
	# output$rough_BT1 <- renderPlot({
	# 	plot_rough_BT()
	# })
	# 
	# # Roughness, Time Varying Box Plot2:
	# output$rough_BT2_UIO <- renderUI({
	# 	if (is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("rough_BT2",
	# 							 height = plot2_sizeTab2()[[length(input$AnalCrit)]])
	# 	}
	# })
	# 
	# output$rough_BT2 <- renderPlot({
	# 	if (is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plot_rough_BT() + facet_wrap(~ facet)
	# 		}
	# })
	# 
	
	# Roughness, General Trend:
	
	plot_sizeTab3 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab3),
												 as.numeric(input$PlotSizeTab3),
												 2 * as.numeric(input$PlotSizeTab3),
												 as.numeric(input$PlotSizeTab3))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_rough_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Roughness,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				stat_summary(fun.y = mean, geom = "point", size = 3, alpha = 1/3) +
				geom_smooth(aes(group = criteria), se = FALSE) +
				# stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 # size = 1) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	# Roughness, General Trend, Plot1:
	output$rough_line1_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_line1",
								 height = plot_sizeTab3()[[4]])
		}
	})
	
	output$rough_line1 <- renderPlot({
		plot_rough_line()
	})
	
	# Roughness, General Trend, Plot2:
	output$rough_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("rough_line2",
								 height = plot_sizeTab3()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$rough_line2 <- renderPlot({
		if (is.null(plot_rough_line())) {
			return(NULL)
		} else {
			plot_rough_line() +
				facet_wrap(~ facet)
		}
	})
	
	# Gained Roughness:
	
	plotG_sizeTab3 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotGSizeTab3),
												 as.numeric(input$PlotGSizeTab3),
												 2 * as.numeric(input$PlotGSizeTab3),
												 as.numeric(input$PlotGSizeTab3))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plotG_rough_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = GainedR_mean,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				scale_y_continuous("Average Gained Roughness") +
				stat_summary(fun.y = mean, geom = "point", size = 3,alpha = 1/3, na.rm = TRUE) +
				# stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 # size = 1, na.rm = TRUE) +
				geom_smooth(aes(group = criteria), se = FALSE) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	

	# Gained Roughness, Plot1:
	
	output$roughG_line1_UIO <- renderUI({
		plotOutput("roughG_line1",
							 height = plotG_sizeTab3()[[4]])
	})
	
	output$roughG_line1 <- renderPlot({
		plotG_rough_line()
	})
	
	# Gained Roughness, Plot2:
	
	output$roughG_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("roughG_line2",
								 height = plotG_sizeTab3()[[length(input$AnalCrit)]])
		}
	})
	
	output$roughG_line2 <- renderPlot({
		if (is.null(plotG_rough_line())) {
			return(NULL)
		} else {
			plotG_rough_line() +
				facet_wrap(~ facet)
		}
	})
	
	# Roughness Model Trends
	
	plot_sizeTabRM <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTabRM),
												 as.numeric(input$PlotSizeTabRM),
												 2 * as.numeric(input$PlotSizeTabRM),
												 as.numeric(input$PlotSizeTabRM))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_rough_model <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = Time,
									 y = Roughness,
									 colour = criteria)) +
				scale_x_continuous("Time", breaks = c(0,90,180,270,360)) +
				geom_smooth(lwd = 1, se = FALSE) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	# Smooth Roughness Model Trend Plot 1:
	
	output$rough_model1_UIO <- renderUI({
		plotOutput("rough_model1",
							 height = plot_sizeTabRM()[[4]])
	})
	
	output$rough_model1 <- renderPlot({
		plot_rough_model()
	})
	
	# Smooth Roughness Model Trend Plot2:
	output$rough_model2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("rough_model2",
								 height = plot_sizeTabRM()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$rough_model2 <- renderPlot({
		if (is.null(plot_rough_model())) {
			return(NULL)
		} else {
			plot_rough_model() +
				facet_wrap(~ facet)
		}
	})
	
	# # Roughness Gained, Model Trend:
	# 
	# 
	# plotG_sizeTabRM <- reactive({
	# 	str_c(as.character(c(2 * as.numeric(input$PlotGSizeTabRM),
	# 											 as.numeric(input$PlotGSizeTabRM),
	# 											 2 * as.numeric(input$PlotGSizeTabRM),
	# 											 as.numeric(input$PlotGSizeTabRM))),
	# 				c("px", "px", "px", "px"), sep = "")
	# })
	# 
	# plotG_rough_model <- reactive({
	# 	if (is.null(Mutated())) {
	# 		return(NULL)
	# 	} else {
	# 		Mutated() %>%
	# 			filter(!is.na(Roughness)) %>%
	# 			ggplot(aes(x = Time, y = GainedR_mean,
	# 								 colour = criteria)) +
	# 			scale_x_continuous ("Time",
	# 													breaks = c(0, 30, 90, 180, 270, 360)) +
	# 			scale_y_continuous("General Gained Roughness") +
	# 			geom_smooth(lwd = 1, se = FALSE) +
	# 			scale_colour_manual (values = Colour()) +
	# 			theme(plot.subtitle = element_text(vjust = 1),
	# 						plot.caption = element_text(vjust = 1),
	# 						panel.grid.major = element_line(colour = "gray5",
	# 																						linetype = "longdash"),
	# 						panel.grid.minor = element_line(colour = "gray5",
	# 																						linetype = "dotdash"),
	# 						panel.background = element_rect(fill = "gray100"),
	# 						axis.text = element_text(colour = "gray5"))
	# 	}
	# })
	# 
	# # Roughness Gained, Model Plot1
	# 
	# output$roughG_model1_UIO <- renderUI({
	# 	plotOutput("roughG_model1",
	# 						 height = plotG_sizeTabRM()[[4]])
	# })
	# 
	# output$roughG_model1 <- renderPlot({
	# 	plotG_rough_model()
	# })
	# 
	# # Roughness Gained, Model Plot2
	# 
	# output$roughG_model2_UIO <- renderUI({
	# 	if (is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("roughG_model2",
	# 							 height = plotG_sizeTabRM()[[length(input$AnalCrit)]])
	# 	}
	# })
	# 
	# output$roughG_model2 <- renderPlot({
	# 	if (is.null(plotG_rough_model())) {
	# 		return(NULL)
	# 	} else {
	# 		plotG_rough_model() +
	# 			facet_wrap(~ facet)
	# 	}
	# })
	
	# Hardness, Jitters:
	
	plot_sizeTab4 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab4),
												 as.numeric(input$PlotSizeTab4),
												 2 * as.numeric(input$PlotSizeTab4),
												 as.numeric(input$PlotSizeTab4))),
					c("px", "px", "px","px"), sep = "")
	})
	
	plot_hard_jitter <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Hardness)) %>%
				ggplot(aes(x = as_factor(as.character (Time)),
									 y = Hardness, colour = criteria)) +
				scale_x_discrete("Time") +
				geom_jitter( position = position_jitter(width = 0.1, height = 0),
										 alpha = 1/3, size = 3) + 
				# geom_smooth(aes(group = criteria), se = FALSE) +
				stat_summary(fun.y = "mean", geom = "line",
										 size = 1, alpha = 1/2,
										 linetype="longdash",
										 aes(group = criteria)) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))	
		}
		
	})
	
	# Jitter1:
	output$hard_jitter1_UIO <- renderUI({
		plotOutput("hard_jitter1",
							 height = plot_sizeTab4()[[4]])
	})
	
	output$hard_jitter1 <- renderPlot({
		plot_hard_jitter()
	})
	
	# Jitter2:
	output$hard_jitter2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hard_jitter2",
								 height = plot_sizeTab4()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_jitter2 <- renderPlot({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plot_hard_jitter() + facet_wrap( ~ facet)
		}
	})
	
	# Hardness, Boxplots:
	
	plot_sizeTab5 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab5),
												 as.numeric(input$PlotSizeTab5),
												 2 * as.numeric(input$PlotSizeTab5),
												 as.numeric(input$PlotSizeTab5))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_hard_box <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>% 
				filter(!is.na(Hardness)) %>% 
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Hardness, colour = criteria)) +
				scale_x_discrete("Time") +
				geom_boxplot(size = 0.75) + 
				stat_summary(fun.y = mean, geom = "point", size = 2) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	# Boxplot1:
	output$hard_box1_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("hard_box1",
								 height = plot_sizeTab5()[[4]])
		}
	})
	
	output$hard_box1 <- renderPlot({
		plot_hard_box()
	})
	
	# Boxplot2:
	output$hard_box2_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("hard_box2",
								 height = plot_sizeTab5()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_box2 <- renderPlot({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plot_hard_box() +
				facet_wrap(~ facet)
		}
	})
	
	# Hradness Trends:
	plot_sizeTab6 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab6),
												 as.numeric(input$PlotSizeTab6),
												 2 * as.numeric(input$PlotSizeTab6),
												 as.numeric(input$PlotSizeTab6))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_hard_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Hardness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Hardness,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				scale_colour_manual (values = Colour()) +
				stat_summary(fun.y = mean, geom = "point", size = 3, alpha = 1/3) +
				# stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = criteria)) +
				geom_smooth(lwd = 1, se = FALSE, aes(group = criteria)) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	# Hardness, Trends Plot1:
	
	output$hard_line1_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hard_line1",
								 height = plot_sizeTab6()[[4]])
		}
	})
	
	output$hard_line1 <- renderPlot({
		plot_hard_line()
	})
	
	
	# Hardness, Trends Plot2:
	
	output$hard_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hard_line2",
								 height = plot_sizeTab6()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_line2 <- renderPlot({
		if (is.null(plot_hard_line())) {
			return(NULL)
		} else {
			plot_hard_line() + 
				facet_wrap(~ facet)
		}
	})
	# Hardness Trends, Gained Hardness:
	
	plotG_sizeTab6 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotGSizeTab6),
												 as.numeric(input$PlotGSizeTab6),
												 2 * as.numeric(input$PlotGSizeTab6),
												 as.numeric(input$PlotGSizeTab6))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plotG_hard_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Hardness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = GainedH_mean,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				scale_y_continuous("Average Gained Hradness") +
				stat_summary(fun.y = mean, geom = "point", 
										 size = 3,alpha = 1/3, na.rm = TRUE) +
				# stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
				# size = 1, na.rm = TRUE) +
				geom_smooth(aes(group = criteria), se = FALSE) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	
	# Gained Hardness, Plot1:

	output$hardG_line1_UIO <- renderUI({
		plotOutput("hardG_line1",
							 height = plotG_sizeTab6()[[4]])
	})
	
	output$hardG_line1 <- renderPlot({
		plotG_hard_line()
	})

	# Gained Hardness, Plot2:

	output$hardG_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hardG_line2",
								 height = plotG_sizeTab6()[[length(input$AnalCrit)]])
		}
	})

	output$hardG_line2 <- renderPlot({
		if (is.null(plotG_hard_line())) {
			return(NULL)
		} else {
			plotG_hard_line() +
				facet_wrap(~ facet)
		}
	})
	
	
	
	# Hardness, Trend for the model:
	
	plot_sizeTabHM <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTabHM),
												 as.numeric(input$PlotSizeTabHM),
												 2 * as.numeric(input$PlotSizeTabHM),
												 as.numeric(input$PlotSizeTabHM))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_hard_model <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Hardness)) %>%
				ggplot(aes(x = Time,
									 y = Hardness,
									 colour = criteria)) +
				scale_x_continuous("Time", breaks = c(0,90,180,270,360)) +
				geom_smooth(lwd = 1, se = FALSE) +
				scale_colour_manual (values = Colour()) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	# Smooth Hardness Model Trend Plot 1:
	
	output$hard_model1_UIO <- renderUI({
		plotOutput("hard_model1",
							 height = plot_sizeTabHM()[[4]])
	})
	
	output$hard_model1 <- renderPlot({
		plot_hard_model()
	})
	
	# Smooth Hardness Model Trend Plot2:
	output$hard_model2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hard_model2",
								 height = plot_sizeTabHM()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$hard_model2 <- renderPlot({
		if (is.null(plot_hard_model())) {
			return(NULL)
		} else {
			plot_hard_model() +
				facet_wrap(~ facet)
		}
	})
	
	# # Hardness Gained, Model Trend:
	# 
	# 
	# plotG_sizeTabHM <- reactive({
	# 	str_c(as.character(c(2 * as.numeric(input$PlotGSizeTabHM),
	# 											 as.numeric(input$PlotGSizeTabHM),
	# 											 2 * as.numeric(input$PlotGSizeTabHM),
	# 											 as.numeric(input$PlotGSizeTabHM))),
	# 				c("px", "px", "px", "px"), sep = "")
	# })
	# 
	# plotG_hard_model <- reactive({
	# 	if (is.null(Mutated())) {
	# 		return(NULL)
	# 	} else {
	# 		Mutated() %>%
	# 			filter(!is.na(Hardness)) %>%
	# 			ggplot(aes(x = Time, y = GainedH_mean,
	# 								 colour = criteria)) +
	# 			scale_x_continuous ("Time",
	# 													breaks = c(0, 30, 90, 180, 270, 360)) +
	# 			scale_y_continuous("General Gained Hardness") +
	# 			geom_smooth(lwd = 1, se = FALSE) +
	# 			scale_colour_manual (values = Colour()) +
	# 			theme(plot.subtitle = element_text(vjust = 1),
	# 						plot.caption = element_text(vjust = 1),
	# 						panel.grid.major = element_line(colour = "gray5",
	# 																						linetype = "longdash"),
	# 						panel.grid.minor = element_line(colour = "gray5",
	# 																						linetype = "dotdash"),
	# 						panel.background = element_rect(fill = "gray100"),
	# 						axis.text = element_text(colour = "gray5"))
	# 	}
	# })
	# 
	# # Hardness Gained, Model Plot1
	# 
	# output$hardG_model1_UIO <- renderUI({
	# 	plotOutput("hardG_model1",
	# 						 height = plotG_sizeTabHM()[[4]])
	# })
	# 
	# output$hardG_model1 <- renderPlot({
	# 	plotG_hard_model()
	# })
	# 
	# # Hardness Gained, Model Plot2
	# 
	# output$hardG_model2_UIO <- renderUI({
	# 	if (is.null(input$AnalCrit)) {
	# 		return(NULL)
	# 	} else {
	# 		plotOutput("hardG_model2",
	# 							 height = plotG_sizeTabHM()[[length(input$AnalCrit)]])
	# 	}
	# })
	# 
	# output$hardG_model2 <- renderPlot({
	# 	if (is.null(plotG_hard_model())) {
	# 		return(NULL)
	# 	} else {
	# 		plotG_hard_model() +
	# 			facet_wrap(~ facet)
	# 	}
	# })
	
	# Flexural Stress:
	# Flexural Stress, Scatter Plots:
	
	
	plot_sizeTab7 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab7),
												 as.numeric(input$PlotSizeTab7),
												 2 * as.numeric(input$PlotSizeTab7),
												 as.numeric(input$PlotSizeTab7))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_flexural_jitter <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Max_Flexural_Stress,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				geom_jitter( position = position_jitter(width = 0.1, height = 0),
										 alpha = 1/3, size = 3) + 
				stat_summary(fun.y = "mean", geom = "line",
										 size = 1, alpha = 1/2,
										 linetype="longdash",
										 aes(group = criteria)) +
				scale_colour_manual (values = unique(filter(colour, material != "R",
																										coating != "PR") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))	
		}
	})
	
	output$flexural_jitter1_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else{
			plotOutput("flexural_jitter1",
								 height = plot_sizeTab7()[[4]])
		}
	})
	
	output$flexural_jitter1 <- renderPlot({
		plot_flexural_jitter()
	})
	
	
	# Flexural Scatter Plot2:
	output$flexural_jitter2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("flexural_jitter2",
								 height = plot_sizeTab7()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$flexural_jitter2 <- renderPlot({
		if (is.null(plot_flexural_jitter())) {
			
		} else{
			plot_flexural_jitter() +
				facet_wrap(~ facet)
		}
	})
	
	# Flexural, Box Plots:
	
	plot_sizeTab8 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab8),
												 as.numeric(input$PlotSizeTab8),
												 2 * as.numeric(input$PlotSizeTab8),
												 as.numeric(input$PlotSizeTab8))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_flexural_box <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = as_factor(as.character(Time)),
									 y = Max_Flexural_Stress,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				geom_boxplot(size = 0.75) +
				stat_summary(fun.y = mean, geom = "point", size = 2) +
				scale_colour_manual (values = unique(filter(colour, material != "R",
																										coating != "PR") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	output$flexural_box1_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("flexural_box1", height = plot_sizeTab8()[[4]])
		}
	})
	
	output$flexural_box1 <- renderPlot({
		plot_flexural_box()
	})
	
	# Box Plot2:
	
	output$flexural_box2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("flexural_box2",
								 height = plot_sizeTab8()[[length(input$AnalCrit)]])
		}
	})
	
	output$flexural_box2 <- renderPlot({
		if (is.null(plot_flexural_box())){
			return(NULL)
		} else {
			plot_flexural_box() +
				facet_wrap(~ facet)
		}
	})
	
	# Flexural Strength, Average Trends:
	# Average Plot1:
	
	plot_sizeTab9 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTab9),
												 as.numeric(input$PlotSizeTab9),
												 2 * as.numeric(input$PlotSizeTab9),
												 as.numeric(input$PlotSizeTab9))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_flexural_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = Max_Flexural_Stress,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				stat_summary(fun.y = mean, geom = "point", size = 3, na.rm = TRUE) +
				# geom_smooth(aes(group = criteria), se = FALSE, na.rm = TRUE) +
				stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 size = 1) +
				
				scale_colour_manual (values = 
														 	unique(filter(colour, material != "R") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	output$flexural_line1_UIO <- renderUI({
		plotOutput("flexural_line1",
							 height = plot_sizeTab9()[[4]])
	})
	
	output$flexural_line1 <- renderPlot({
		plot_flexural_line()
	})
	
	# Average Plot2:
	output$flexural_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("flexural_line2",
								 height = plot_sizeTab9()[[length(input$AnalCrit)]])
		}
	})
	
	output$flexural_line2 <- renderPlot({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plot_flexural_line() +
				facet_wrap(~ facet)
		}
	})
	
	# Flexural Trends, Gained Strength:
	
	plotG_sizeTab9 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotGSizeTab9),
												 as.numeric(input$PlotGSizeTab9),
												 2 * as.numeric(input$PlotGSizeTab9),
												 as.numeric(input$PlotGSizeTab9))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plotG_flexural_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = GainedF_mean,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				scale_y_continuous("Average Gained Flexural Strength") +
				stat_summary(fun.y = mean, geom = "point", 
										 size = 3,alpha = 1/3, na.rm = TRUE) +
				stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
				size = 1, na.rm = TRUE) +
				# geom_smooth(aes(group = criteria), se = FALSE) +
				scale_colour_manual (values = 
														 	unique(filter(colour, material != "R") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	
	# Gained Flexural Strength, Plot1:
	
	output$flexuralG_line1_UIO <- renderUI({
		plotOutput("flexuralG_line1",
							 height = plotG_sizeTab9()[[4]])
	})
	
	output$flexuralG_line1 <- renderPlot({
		plotG_flexural_line()
	})
	
	# Gained Flexural Strength, Plot2:
	
	output$flexuralG_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("flexuralG_line2",
								 height = plotG_sizeTab9()[[length(input$AnalCrit)]])
		}
	})
	
	output$flexuralG_line2 <- renderPlot({
		if (is.null(plotG_flexural_line())) {
			return(NULL)
		} else {
			plotG_flexural_line() +
				facet_wrap(~ facet)
		}
	})
	
	# Flexural Strength, Trend for the Model
	plot_sizeTabFM <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotSizeTabFM),
												 as.numeric(input$PlotSizeTabFM),
												 2 * as.numeric(input$PlotSizeTabFM),
												 as.numeric(input$PlotSizeTabFM))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	plot_flexural_model <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = Time,
									 y = Max_Flexural_Stress,
									 colour = criteria)) +
				scale_x_continuous("Time", breaks = c(0,90,180,270,360)) +
				stat_summary(fun.y = mean, geom = "point", size = 3, na.rm = TRUE) +
				geom_smooth(lwd = 1, se = FALSE) +
				scale_colour_manual (values = 
														 	unique(filter(colour, material != "R") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})
	
	# Smooth, Flexural Strength Model Trend, Plot 1:
	
	output$flexural_model1_UIO <- renderUI({
		plotOutput("flexural_model1",
							 height = plot_sizeTabFM()[[4]])
	})
	
	output$flexural_model1 <- renderPlot({
		plot_flexural_model()
	})
	
	# Smooth, Flexural Model Trend, Plot2:
	output$flexural_model2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("flexural_model2",
								 height = plot_sizeTabFM()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$flexural_model2 <- renderPlot({
		if (is.null(plot_flexural_model())) {
			return(NULL)
		} else {
			plot_flexural_model() +
				facet_wrap(~ facet)
		}
	})
	
	# Flexural Strength Gained, Model Trend:


	plotG_sizeTabFM <- reactive({
		str_c(as.character(c(2 * as.numeric(input$PlotGSizeTabFM),
												 as.numeric(input$PlotGSizeTabFM),
												 2 * as.numeric(input$PlotGSizeTabFM),
												 as.numeric(input$PlotGSizeTabFM))),
					c("px", "px", "px", "px"), sep = "")
	})

	plotG_flexural_model <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Max_Flexural_Stress)) %>%
				ggplot(aes(x = Time, y = GainedF_mean,
									 colour = criteria)) +
				scale_x_continuous ("Time",
														breaks = c(0, 30, 90, 180, 270, 360)) +
				scale_y_continuous("Flexural Strength Gained") +
				geom_smooth(lwd = 1, se = FALSE) +
				stat_summary(fun.y = mean, geom = "point", size = 3, na.rm = TRUE) +
	scale_colour_manual (values =
											 	unique(filter(colour, material != "R") [[colour_scheme()]])) +
				theme(plot.subtitle = element_text(vjust = 1),
							plot.caption = element_text(vjust = 1),
							panel.grid.major = element_line(colour = "gray5",
																							linetype = "longdash"),
							panel.grid.minor = element_line(colour = "gray5",
																							linetype = "dotdash"),
							panel.background = element_rect(fill = "gray100"),
							axis.text = element_text(colour = "gray5"))
		}
	})

	# Flexural Strength Gained, Model Plot1

	output$flexuralG_model1_UIO <- renderUI({
		plotOutput("flexuralG_model1",
							 height = plotG_sizeTabFM()[[4]])
	})

	output$flexuralG_model1 <- renderPlot({
		plotG_flexural_model()
	})

	# Flexural Strength Gained, Model Plot2:

	output$flexuralG_model2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("flexuralG_model2",
								 height = plotG_sizeTabFM()[[length(input$AnalCrit)]])
		}
	})

	output$flexuralG_model2 <- renderPlot({
		if (is.null(plotG_flexural_model())) {
			return(NULL)
		} else {
			plotG_flexural_model() +
				facet_wrap(~ facet)
		}
	})
	
	# # unique(filter(colour, material != "R", coating != "PR") [[colour_scheme()]])
	
	
}