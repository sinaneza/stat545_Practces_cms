# UI:
ui <- fluidPage(
	br(),
	titlePanel("Environmental Effect on Fibre Glass Composites, Exposed Samples"),
	fluidRow(
		br(), br(),
		column(3,
					 br(), br(),br(),br(), br(),br(), br(), br(),
					 wellPanel(
					 	checkboxGroupInput("AnalCrit", "Analysis Criteria:",
					 										 choices = c("Material", "Coating", "Cure"),
					 										 selected ="Material")
					 )
		),
		column(9,
					 tabsetPanel(
					 	tabPanel("Roughness Scatter Plots",
					 					 br(),
					 					 h2("Exposed Samples, Roughness Scatter Plots"),
					 					 br(),
					 					 selectInput("PlotSizeTab1", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput ("rough_jitter1_UIO"),
					 					 br(),br(),
					 					 uiOutput("rough_jitter2_UIO"),
					 					 br(), hr()
					 	),
					 	# tabPanel("Roughness, Time Varying ScatterPlots",
					 	# 				 br(),
					 	# 				 h2("Exposed Samples, Roughness, Time Varying Scatter Plots"),
					 	# 				 br(),
					 	# 				 uiOutput ("rough_JT1_UIO"),
					 	# 				 br(),
					 	# 				 uiOutput ("rough_JT2_UIO"),
					 	# 				 hr(),
					 	# 				 fluidRow(
					 	# 				 	column(4, selectInput("Plot2SizeTab1", "Heghit of The Graph in Pixels:",
					 	# 				 												choices = seq(100, 1000, 50), selected = 250)),
					 	# 				 	column(2),
					 	# 				 	column(5, sliderInput("TimeRJ", label="Time in Days (0, 30, 90, 180, 270, 360):
					 	# 				 												0 = Day 0, 
					 	# 				 												1 = Day 30,
					 	# 				 												2 = Day 90,
					 	# 				 												3 = Day 180,
					 	# 				 												4 = Day 270,
					 	# 				 												5 = Day 360",
					 	# 				 												min = 0, max = 5, step = 1, value = 0)))
					 	# 				 ),
					 	tabPanel("Roughness Box Plots", 
					 					 br(),
					 					 h2("Exposed Samples, Roughness Box Plots:"),
					 					 br(),
					 					 selectInput("PlotSizeTab2", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput("rough_box1_UIO"), br(), br(),
					 					 uiOutput("rough_box2_UIO"), br(), br()
					 	),
					 	# tabPanel("Roughness, Time Varying Box Plots",
					 	# 				 br(),
					 	# 				 h2("Exposed Samples, Roughness, Time Varying Box Plots"),
					 	# 				 br(),
					 	# 				 uiOutput ("rough_BT1_UIO"),
					 	# 				 br(),
					 	# 				 uiOutput ("rough_BT2_UIO"),
					 	# 				 hr(),
					 	# 				 fluidRow(
					 	# 				 	column(4, selectInput("Plot2SizeTab2", "Heghit of The Graph in Pixels:",
					 	# 				 												choices = seq(100, 1000, 50), selected = 250)),
					 	# 				 	column(2),
					 	# 				 	column(5, sliderInput("TimeRB", label="Time in Days (0, 30, 90, 180, 270, 360):
					 	# 				 												0 = Day 0, 
					 	# 				 												1 = Day 30,
					 	# 				 												2 = Day 90,
					 	# 				 												3 = Day 180,
					 	# 				 												4 = Day 270,
					 	# 				 												5 = Day 360",
					 	# 				 												min = 0, max = 5, step = 1, value = 0))
					 	# 				 	
					 	# 				 )
					 	# ),
					 	tabPanel("Roughness Average Trends",
					 					 br(),
					 					 h2("Exposed Samples, Roughness Average Trends:"),
					 					 br(),
					 					 selectInput("PlotSizeTab3", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput("rough_line1_UIO"),
					 					 br(), br(),
					 					 uiOutput("rough_line2_UIO"),
					 					 br(), br()
					 	),
					 	tabPanel("Hardness Scatter Plots:",
					 					 br(),
					 					 h2("Exposed Samples, Hardness, Scatter Plots:"),
					 					 br(),
					 					 selectInput("PlotSizeTab4", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput ("hard_jitter1_UIO"),
					 					 br(),br(),
					 					 uiOutput("hard_jitter2_UIO"),
					 					 br(), br()
					 	),
					 	tabPanel("Hardness Box Plots:",
					 					 br(),
					 					 h2("Exposed Samples, Hardness, Box Plots:"),
					 					 br(),
					 					 selectInput("PlotSizeTab5", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput ("hard_box1_UIO"),
					 					 br(),br(),
					 					 uiOutput("hard_box2_UIO"),
					 					 br(), br()
					 	),
					 	tabPanel("Hardness Average Trends",
					 					 br(),
					 					 h2("Exposed Samples, Hardness Average Trends:"),
					 					 br(),
					 					 selectInput("PlotSizeTab6", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput("hard_line1_UIO"),
					 					 br(), br(),
					 					 uiOutput("hard_line2_UIO"),
					 					 br(), br()
					 	),
					 	tabPanel("Flexural Strength Scatter Plots",
					 					 br(),
					 					 h2("Exposed Samples, Flexural Stress Scatter Plots"),
					 					 br(),
					 					 selectInput("PlotSizeTab7", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput ("flexural_jitter1_UIO"),
					 					 br(),br(),
					 					 uiOutput("flexural_jitter2_UIO"),
					 					 br(), br()
					 	),
					 	tabPanel("Flexural Strength Box Plots", 
					 					 br(),
					 					 h2("Exposed Samples, Flexural Strength Box Plots:"),
					 					 br(),
					 					 selectInput("PlotSizeTab8", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput("flexural_box1_UIO"), 
					 					 br(), br(),
					 					 uiOutput("flexural_box2_UIO"), 
					 					 br(), br()
					 	),
					 	tabPanel("Flexural Strength Average Trends",
					 					 br(),
					 					 h2("Exposed Samples, Flexural Strength, Average Trends:"),
					 					 br(),
					 					 selectInput("PlotSizeTab9", "Heghit of Plots in Pixels:",
					 					 						choices = seq(100, 1000, 50), selected = 400),
					 					 hr(),
					 					 uiOutput("flexural_line1_UIO"),
					 					 br(), br(),
					 					 uiOutput("flexural_line2_UIO"),
					 					 br(), br()
					 	)
					 )
		)
	)
)


# Server:

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
	
	
	
	
	
	
	# First Scatter Plot:
	
	
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
								 height = plot_sizeTab1()[[4]])
		}
	})
	
	
	output$rough_jitter1 <- renderPlot({
		plot_rough_jitter()
	})
	
	# Scatter Plot2:
	
	
	output$rough_jitter2_UIO <- renderUI({
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
	
	# Scatter Plot, Varying Time:
	
	plot2_sizeTab1 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$Plot2SizeTab1),
												 as.numeric(input$Plot2SizeTab1),
												 2 * as.numeric(input$Plot2SizeTab1),
												 as.numeric(input$Plot2SizeTab1))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	filtered <- reactive({
		if (is.null(Mutated())){
			return(NULL)
		}
		Mutated() %>%
			filter(Time2 == input$TimeRJ)
	})
	
	plot_rough_JT <- reactive({
		if(is.null(filtered())){
			return(NULL)
		} else {
			filtered() %>%
				filter(!is.na(Roughness)) %>%
				mutate(criteria = fct_drop(criteria)
							 # criteria = fct_reorder(criteria, Roughness,
							 # 												 fun = median)
				) %>%
				ggplot(aes(x = criteria, y = Roughness,
									 colour = criteria)) + 
				scale_x_discrete("criteria") +
				scale_y_continuous("Roughness",
													 limits = c(min(Mutated()$Roughness,
													 							 na.rm = TRUE),max(Mutated()$Roughness,
													 							 									na.rm = TRUE))) +
				geom_jitter(position = position_jitter(width = 0.1, height = 0),
										alpha = 1/3, size = 3) + 
				scale_colour_manual(values = Colour()) +
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
	
	# Box Plot, Varying Time:
	
	plot2_sizeTab2 <- reactive({
		str_c(as.character(c(2 * as.numeric(input$Plot2SizeTab2),
												 as.numeric(input$Plot2SizeTab2),
												 2 * as.numeric(input$Plot2SizeTab2),
												 as.numeric(input$Plot2SizeTab2))),
					c("px", "px", "px", "px"), sep = "")
	})
	
	filtered_BT <- reactive({
		if (is.null(Mutated())){
			return(NULL)
		}
		Mutated() %>%
			filter(Time2 == input$TimeRB)
	})
	
	plot_rough_BT <- reactive({
		if(is.null(filtered_BT())){
			return(NULL)
		} else {
			filtered_BT() %>%
				filter(!is.na(Roughness)) %>%
				mutate(criteria = fct_drop(criteria)
							 # criteria = fct_reorder(criteria, Roughness,
							 # 												 fun = median)
				) %>%
				ggplot(aes(x = criteria, y = Roughness,
									 colour = criteria)) +
				scale_x_discrete("criteria") +
				scale_y_continuous("Roughness",
													 limits = c(min(Mutated()$Roughness,
													 							 na.rm = TRUE),
													 					 max(Mutated()$Roughness,
													 					 		na.rm = TRUE))) +
				geom_boxplot() +
				scale_colour_manual(values = Colour()) +
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
	# Average Plot1:
	
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
							 height = plot_sizeTab3()[[4]])
	})
	
	output$rough_line1 <- renderPlot({
		plot_rough_line()
	})
	
	# Average Plot2:
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
	
	# Hradness Average Trennds:
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
	
	output$hard_line2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("hard_line2",
								 height = plot_sizeTab6()[[length(input$AnalCrit)]])
		}
	})
	
	output$hard_line2 <- renderPlot({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plot_hard_line() + 
				facet_wrap(~ facet)
		}
	})
	
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
	
}