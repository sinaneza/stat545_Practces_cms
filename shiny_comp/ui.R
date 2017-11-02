# plot_size <- c("", "400px", "800px")
ui <- fluidPage(
	br(),
	titlePanel("Environmental Effect on Fibre Glass Composites"),
	br(),
	tabsetPanel(
		tabPanel("Exposed Samples, Roughness Scatter Plots",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 	checkboxGroupInput("AnalCrit", "Analysis Criteria:",
						 										 choices = c("Material", "Coating", "Cure"),
						 										 selected ="Material"),
						 	br(), br(),
						 	selectInput("PlotSizeTab1", "Heghit of Plots in Pixels:",
						 							choices = seq(100, 1000, 50), selected = 400),
						 width = 3),
						 							mainPanel(
						 								h2("Exposed Samples, Roughness Scatter Plots"),
						 								uiOutput ("rough_jitter1_UIO"),
						 								br(),br(),
						 								uiOutput("rough_jitter2_UIO"),br(), br(),
						 								width = 9
						 							)
						 )
						 
						 ),
		tabPanel("Exposed Samples,Roughness Box Plots", 
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab2", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")
						 											 ),br(), br(),
						 		selectInput("PlotSizeTab2", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 	),
						 	mainPanel(
						 		h2("Exposed Samples, Roughness Box Plots:"),
						 		uiOutput("rough_box1_UIO"), br(), br(),
						 		uiOutput("rough_box2_UIO"), br(), br(),
						 		width = 9
						 	)
						 	
						 )
						 ),
		tabPanel("Exposed Samples, Roughness Average Trends",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab3", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab3", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 	),
						 	mainPanel(
						 		h2("Exposed Samples, Roughness Average Trends:"),
						 		uiOutput("rough_line1_UIO"),
						 		br(), br(),
						 		uiOutput("rough_line2_UIO"),
						 		br(), br(),
						 		width = 9
						 	)
						 )
						 ),
		tabPanel("Exposed Samples, Hardness Scatter Plots:",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab4", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab4", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3),
						 	mainPanel(
						 		h2("Exposed Samples, Hardness, Scatter Plots:"),
						 		uiOutput ("hard_jitter1_UIO"),
						 		br(),br(),
						 		uiOutput("hard_jitter2_UIO"),br(), br(),
						 		width = 9
						 	)
						 )
						 
		),
		tabPanel("Exposed Samples, Hardness Box Plots:",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab5", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab5", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 		),
						 	mainPanel(
						 		h2("Exposed Samples, Hardness, Box Plots:"),
						 		uiOutput ("hard_box1_UIO"),
						 		br(),br(),
						 		uiOutput("hard_box2_UIO"),br(), br(),
						 	width = 9)
						 )
		),
		tabPanel("Exposed Samples, Hardness Average Trends",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab6", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab6", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 	),
						 	mainPanel(
						 		h2("Exposed Samples, Hardness Average Trends:"),
						 		uiOutput("hard_line1_UIO"),
						 		br(), br(),
						 		uiOutput("hard_line2_UIO"),
						 		br(), br(),
						 		width = 9
						 	)
						 )
		),
		tabPanel("Exposed Samples, Flexural Strength, Scatter Plots",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab7", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab7", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3),
						 	mainPanel(
						 		h2("Exposed Samples, Flexural Stress Scatter Plots"),
						 		uiOutput ("flexural_jitter1_UIO"),
						 		br(),br(),
						 		uiOutput("flexural_jitter2_UIO"),br(), br(),
						 		width = 9
						 	)
						 )
		),
		tabPanel("Exposed Samples,Flexural Strength Box Plots", 
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab8", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")
						 		),br(), br(),
						 		selectInput("PlotSizeTab8", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 	),
						 	mainPanel(
						 		h2("Exposed Samples, Flexural Strength Box Plots:"),
						 		uiOutput("flexural_box1_UIO"), br(), br(),
						 		uiOutput("flexural_box2_UIO"), br(), br(),
						 		width = 9
						 	)
						 	
						 )
		),
		tabPanel("Exposed Samples, Flexural Strength, Average Trends",
						 br(), br(),
						 sidebarLayout(
						 	sidebarPanel(
						 		checkboxGroupInput("AnalCritTab9", "Analysis Criteria:",
						 											 choices = c("Material", "Coating", "Cure")),
						 		br(), br(),
						 		selectInput("PlotSizeTab9", "Heghit of Plots in Pixels:",
						 								choices = seq(100, 1000, 50), selected = 400),
						 		width = 3
						 	),
						 	mainPanel(
						 		h2("Exposed Samples, Flexural Strength, Average Trends:"),
						 		uiOutput("flexural_line1_UIO"),
						 		br(), br(),
						 		uiOutput("flexural_line2_UIO"),
						 		br(), br(),
						 		width = 9
						 	)
						 )
		)
	)
	
	
	
	
	
)
