
server <- function(input, output, session) {
	library(tidyverse)
	library(stringr)
	library(forcats)
	
	
	
	


	
	Mutated <- reactive({if (is.null(input$AnalCrit)) {
		return (NULL)
	} else {
		Mutate_Vars(Exposed, input$AnalCrit)
	}
		})
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
	
	



	
	# First Scatter Plot:
	
	plot_sizeTab1 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab1),
												 as.numeric(input$PlotSizeTab1),
												 2 * as.numeric(input$PlotSizeTab1))),
					c("px", "px", "px"), sep = "")
	})
	
	plot_rough_jitter <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = as_factor(as.character (Time)), y = Roughness, colour = criteria)) +
				scale_x_discrete("Time") +
				geom_jitter( position = position_jitter(width = 0.1, height = 0),
										 alpha = 1/3, size = 3) + 
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
	
	output$rough_jitter1_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else{
			plotOutput("rough_jitter1",
								 height = plot_sizeTab1()[[1]])
		}
	})
	
	
	output$rough_jitter1 <- renderPlot({
		plot_rough_jitter()
	})
	
	# Scatter Plot2:
	output$rough_jitter2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else if (length(input$AnalCrit) > 1) {
			plotOutput("rough_jitter2",
								 height = plot_sizeTab1()[[length(input$AnalCrit)]])
		}
	})
	
	
	output$rough_jitter2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_rough_jitter() +
				facet_wrap(~ plot_rough_jitter()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
			} else if (length(input$AnalCrit) == 3) {
				plot_rough_jitter() +
					facet_wrap(~ plot_rough_jitter()$data$crit_3)
				}
	})
	
	# Box Plot1:

	observe({
		updateCheckboxGroupInput(session, "AnalCritTab2", selected = input$AnalCrit)
	})
	
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab2)
	})
	
	plot_sizeTab2 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab2),
												 as.numeric(input$PlotSizeTab2),
												 2 * as.numeric(input$PlotSizeTab2))),
					c("px", "px", "px"), sep = "")
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
		} else if (length(input$AnalCrit) < 3) {
			plotOutput("rough_box1", height = plot_sizeTab2()[[1]])
			}
	})
	

	
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
		} else if (length(input$AnalCrit) > 1){
			plotOutput("rough_box2", height = plot_sizeTab2()[[length(input$AnalCrit)]])
		}
		# if (length(input$AnalCrit) == 1) {
		# 	return(NULL)
		# } else if (length(input$AnalCrit) == 2) {
		# 	plotOutput("rough_box2", height = "400px")
		# } else if (length(input$AnalCrit) == 3) {
		# 	plotOutput("rough_box2", height = "800px")
		# }
	})
	
	output$rough_box2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_rough_box() +
				facet_wrap(~ plot_rough_box()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_rough_box() + 
				facet_wrap(~ plot_rough_box()$data$crit_3)
		}
		# if (is.null(input$AnalCrit)) {
		# 	return (NULL)
		# } else if (length(input$AnalCrit) == 2) {
		# 	plot_rough_box() + 
		# 		facet_wrap(~ plot_rough_box()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		# } else if (length(input$AnalCrit) == 3) {
		# 	plot_rough_box() + 
		# 		facet_wrap(~ plot_rough_box()$data$crit_3)
		# }
	})
	# Average Plot1:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab3", selected = input$AnalCrit)
	})
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab3)
	})
	
	plot_sizeTab3 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab3),
												 as.numeric(input$PlotSizeTab3),
												 2 * as.numeric(input$PlotSizeTab3))),
					c("px", "px", "px"), sep = "")
	})
	
	plot_rough_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Roughness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = Roughness,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				stat_summary(fun.y = mean, geom = "point", size = 3) +
				stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 size = 1) + 
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
	
	output$rough_line1_UIO <- renderUI({
		plotOutput("rough_line1",
							 height = plot_sizeTab3()[[1]])
	})
	
	output$rough_line1 <- renderPlot({
		plot_rough_line()
	})
	
	# Average Plot2:
	output$rough_line2_UIO <- renderUI({
		if (length(input$AnalCrit) > 1) {
			plotOutput("rough_line2", 
								 height = plot_sizeTab3()[[length(input$AnalCrit)]])
		}
		# if (length(input$AnalCrit) == 1) {
		# 	return(NULL)
		# } else if (length(input$AnalCrit) == 2) {
		# 	plotOutput("rough_line2", height = "400px")
		# } else if (length(input$AnalCrit) == 3) {
		# 	plotOutput("rough_line2", height = "800px")
		# }
	})
	
	output$rough_line2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_rough_line() + 
				facet_wrap(~ plot_rough_line()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_rough_line() +
				facet_wrap(~ plot_rough_line()$data$crit_3)
		}
		# if (length(input$AnalCrit) == 1) {
		# 	return(NULL)
		# }
		# else if (length(input$AnalCrit) == 2) {
		# 	plot_rough_line() + 
		# 		facet_wrap(~ plot_rough_line()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		# }
		# else if (length(input$AnalCrit) == 3) {
		# 	plot_rough_line() +
		# 		facet_wrap(~ plot_rough_line()$data$crit_3)
		# }
	})
	
	# Hardness, Jitters:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab4", selected = input$AnalCrit)
	})
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab4)
	})
	
	plot_sizeTab4 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab4),
												 as.numeric(input$PlotSizeTab4),
												 2 * as.numeric(input$PlotSizeTab4))),
					c("px", "px", "px"), sep = "")
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
							 height = plot_sizeTab4()[[1]])
	})
	
	output$hard_jitter1 <- renderPlot({
		plot_hard_jitter()
	})
	
	# Jitter2:
	output$hard_jitter2_UIO <- renderUI({
		if (length(input$AnalCrit) > 1) {
			plotOutput("hard_jitter2",
								 height = plot_sizeTab4()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_jitter2 <- renderPlot({
		if(length(input$AnalCrit) == 2){
			plot_hard_jitter() + 
				facet_wrap(~ plot_hard_jitter()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_hard_jitter() + 
				facet_wrap(~ plot_hard_jitter()$data$crit_3)
		}
	})
	
	# Hardness, Boxplots:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab5", selected = input$AnalCrit)
	})
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab5)
	})
	
	plot_sizeTab5 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab5),
												 as.numeric(input$PlotSizeTab5),
												 2 * as.numeric(input$PlotSizeTab5))),
					c("px", "px", "px"), sep = "")
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
		} else if (length(input$AnalCrit) < 3) {
			plotOutput("hard_box1", 
								 height = plot_sizeTab5()[[1]])
		}
	})
	
	output$hard_box1 <- renderPlot({
		plot_hard_box()
	})
	
	# Boxplot2:
	output$hard_box2_UIO <- renderUI({
		if (length(input$AnalCrit) > 1) {
			plotOutput("hard_box2",
								 height = plot_sizeTab5()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_box2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_hard_box() +
				facet_wrap(~ plot_hard_box()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
			} else if (length(input$AnalCrit) == 3) {
				plot_hard_box() +
					facet_wrap(~ plot_hard_box()$data$crit_3)
				}
		})
		
	# Hradness Average Trennds:
	plot_sizeTab6 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab6),
												 as.numeric(input$PlotSizeTab6),
												 2 * as.numeric(input$PlotSizeTab6))),
					c("px", "px", "px"), sep = "")
	})
	
	plot_hard_line <- reactive({
		if (is.null(Mutated())) {
			return(NULL)
		} else {
			Mutated() %>%
				filter(!is.na(Hardness)) %>%
				ggplot(aes(x = as_factor(as.character(Time)), y = Hardness,
									 colour = criteria)) +
				scale_x_discrete("Time") +
				scale_colour_manual (values = Colour()) +
				stat_summary(fun.y = mean, geom = "point", size = 3) +
				stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 size = 1) + 
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
	# Hardness, Average Plot1:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab6", selected = input$AnalCrit)
	})
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab6)
	})
	
	output$hard_line1_UIO <- renderUI({
		plotOutput("hard_line1",
							 height = plot_sizeTab6()[[1]])
	})
	
	output$hard_line1 <- renderPlot({
		plot_hard_line()
	})
	
	output$hard_line2_UIO <- renderUI({
		if (length(input$AnalCrit) > 1) {
			plotOutput("hard_line2", 
								 height = plot_sizeTab6()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_line2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_hard_line() + 
				facet_wrap(~ plot_hard_line()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_hard_line() +
				facet_wrap(~ plot_hard_line()$data$crit_3)
		}
	})
	
	# Flexural Stress:
	# Flexural Stress, Scatter Plots:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab7", selected = input$AnalCrit)
	})
	
	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab7)
	})
	
	plot_sizeTab7 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab7),
												 as.numeric(input$PlotSizeTab7),
												 2 * as.numeric(input$PlotSizeTab7))),
					c("px", "px", "px"), sep = "")
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
								 height = plot_sizeTab7()[[1]])
		}
	})
	
	output$flexural_jitter1 <- renderPlot({
		plot_flexural_jitter()
	})
	
	
	# Flexural Scatter Plot2:
	output$flexural_jitter2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else if (length(input$AnalCrit) > 1) {
			plotOutput("flexural_jitter2",
								 height = plot_sizeTab7()[[length(input$AnalCrit)]])
		}
	})


	output$flexural_jitter2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_flexural_jitter() +
				facet_wrap(~ plot_flexural_jitter()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_flexural_jitter() +
				facet_wrap(~ plot_flexural_jitter()$data$crit_3)
		}
	})
	
	# Flexural, Box Plots:

	observe({
		updateCheckboxGroupInput(session, "AnalCritTab8", selected = input$AnalCrit)
	})

	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab8)
	})
	
	plot_sizeTab8 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab8),
												 as.numeric(input$PlotSizeTab8),
												 2 * as.numeric(input$PlotSizeTab8))),
					c("px", "px", "px"), sep = "")
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
		} else if (length(input$AnalCrit) < 3) {
			plotOutput("flexural_box1", height = plot_sizeTab8()[[1]])
		}
	})
	
	output$flexural_box1 <- renderPlot({
		plot_flexural_box()
	})
	
	# Box Plot2:

	output$flexural_box2_UIO <- renderUI({
		if (is.null(input$AnalCrit)){
			return(NULL)
		} else if (length(input$AnalCrit) > 1){
			plotOutput("flexural_box2", 
								 height = plot_sizeTab8()[[length(input$AnalCrit)]])
		}
	})
	
	output$flexural_box2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_flexural_box() +
				facet_wrap(~ plot_flexural_box()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_flexural_box() +
				facet_wrap(~ plot_flexural_box()$data$crit_3)
		}
	})
	
	# Flexural Strength, Average Trends:
	# Average Plot1:
	
	observe({
		updateCheckboxGroupInput(session, "AnalCritTab9", selected = input$AnalCrit)
	})

	observe({
		updateCheckboxGroupInput(session, "AnalCrit", selected = input$AnalCritTab9)
	})
	
	plot_sizeTab9 <- reactive({
		str_c(as.character(c(as.numeric(input$PlotSizeTab9),
												 as.numeric(input$PlotSizeTab9),
												 2 * as.numeric(input$PlotSizeTab9))),
					c("px", "px", "px"), sep = "")
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
				stat_summary(fun.y = mean, geom = "point", size = 3) +
				stat_summary(fun.y = mean, geom = "line", aes(group = criteria),
										 size = 1) +
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
	
	output$flexural_line1_UIO <- renderUI({
		plotOutput("flexural_line1",
							 height = plot_sizeTab9()[[1]])
	})

	output$flexural_line1 <- renderPlot({
		plot_flexural_line()
	})

	# Average Plot2:
	output$flexural_line2_UIO <- renderUI({
		if (length(input$AnalCrit) > 1) {
			plotOutput("flexural_line2",
								 height = plot_sizeTab9()[[length(input$AnalCrit)]])
		}
	})
	
	output$flexural_line2 <- renderPlot({
		if (length(input$AnalCrit) == 2) {
			plot_flexural_line() +
				facet_wrap(~ plot_flexural_line()$data[[crit_vars[!(crit_vars %in% input$AnalCrit)]]])
		} else if (length(input$AnalCrit) == 3) {
			plot_flexural_line() +
				facet_wrap(~ plot_flexural_line()$data$crit_3)
		}
	})
	
	
	
	# # unique(filter(colour, material != "R", coating != "PR") [[colour_scheme()]])
	
	
	}