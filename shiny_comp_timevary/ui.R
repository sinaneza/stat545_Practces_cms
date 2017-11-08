ui <- fluidPage(
	br(),
	titlePanel("Environmental Effect on Fibre Glass Composites, Exposed Samples, Time Varying Plots"),
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
					 	tabPanel("Roughness, ScatterPlots",
					 					 br(),
					 					 h2("Exposed Samples, Roughness, Time Varying Scatter Plots"),
					 					 br(),
					 					 uiOutput ("rough_JT1_UIO"),
					 					 br(),
					 					 uiOutput ("rough_JT2_UIO"),
					 					 hr(),
					 					 fluidRow(
					 					 	column(4, selectInput("Plot2SizeTab1", "Heghit of The Graph in Pixels:",
					 					 												choices = seq(100, 1000, 50), selected = 250)),
					 					 	column(2),
					 					 	column(5, sliderInput("TimeRJ", label="Time in Days (0, 30, 90, 180, 270, 360):
					 					 												0 = Day 0,
					 					 												1 = Day 30,
					 					 												2 = Day 90,
					 					 												3 = Day 180,
					 					 												4 = Day 270,
					 					 												5 = Day 360",
					 					 												min = 0, max = 5, step = 1, value = 0)))
					 					 ),
					 	tabPanel("Roughness, Box Plots",
					 					 br(),
					 					 h2("Exposed Samples, Roughness, Time Varying Box Plots"),
					 					 br(),
					 					 uiOutput ("rough_BT1_UIO"),
					 					 br(),
					 					 uiOutput ("rough_BT2_UIO"),
					 					 hr(),
					 					 fluidRow(
					 					 	column(4, selectInput("Plot2SizeTab2", "Heghit of The Graph in Pixels:",
					 					 												choices = seq(100, 1000, 50), selected = 250)),
					 					 	column(2),
					 					 	column(5, sliderInput("TimeRB", label="Time in Days (0, 30, 90, 180, 270, 360):
					 					 												0 = Day 0,
					 					 												1 = Day 30,
					 					 												2 = Day 90,
					 					 												3 = Day 180,
					 					 												4 = Day 270,
					 					 												5 = Day 360",
					 					 												min = 0, max = 5, step = 1, value = 0))
					 					 )
					 	)
					 )
		)
	)
)
