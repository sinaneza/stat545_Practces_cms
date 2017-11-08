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
	output$rough_JT1_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_JT1",
								 height = plot2_sizeTab1()[[4]])
		}
	})
	output$rough_JT1 <- renderPlot({
		plot_rough_JT()
	})


	# Roughness, Time Varying Scatter Plot2:
	output$rough_JT2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("rough_JT2",
								 height = plot2_sizeTab1()[[length(input$AnalCrit)]])
		}
	})

	output$rough_JT2 <- renderPlot({
		if(is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plot_rough_JT() + facet_wrap(~ facet)
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
	
	# Roughness, Time Varying Box Plot1:

	output$rough_BT1_UIO <- renderUI({
		if(is.null(input$AnalCrit)){
			return(NULL)
		} else {
			plotOutput("rough_BT1",
								 height = plot2_sizeTab2()[[4]])
		}
	})
	output$rough_BT1 <- renderPlot({
		plot_rough_BT()
	})

	# Roughness, Time Varying Box Plot2:
	output$rough_BT2_UIO <- renderUI({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plotOutput("rough_BT2",
								 height = plot2_sizeTab2()[[length(input$AnalCrit)]])
		}
	})

	output$rough_BT2 <- renderPlot({
		if (is.null(input$AnalCrit)) {
			return(NULL)
		} else {
			plot_rough_BT() + facet_wrap(~ facet)
			}
	})
	
}
