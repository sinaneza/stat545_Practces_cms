# Outputs should be buit through the server function
## There are three rules to be followed in creating outputs
	# 1) Save output objects into `output` list
	# 2) Build each output object using render* (* defines the type of output)
	# 3) Use 'input' list for input
# Attention: There two lists named 'input' and 'output' as arguments of server function.

# A sample function producing output regardless of input:
# server <- function(input, output) {
# 	output$coolplot <- renderPlot({
# 		plot(rnorm(100))
# 	})
# }

# Server has an input list, defined by the UI.
# If server function is using input list to produce output, any time that the user change the input,
# output is updated, this is called reactivity.
# Following function is an example, it provides random values in quantity of 
# defined minimum price (doesnt make sence)

# server <- function(input, output){
# 	output$coolplot <- renderPlot({
# 		plot(rnorm(input$priceInput[1]))
# 	})
# }


## Providing the histogram of alchohol content for the whole data
	# This plot does not change by the changeof input, since it is for the whole data.

	# server <- function(input, output){
	# 	output$coolplot <- renderPlot({
	# 		ggplot(bcl, aes(x = Alcohol_Content)) +
	# 			geom_histogram()
	# 	})

# Now I want to folter the data regarding the input

## reactive variable
	# All input variables in shiny are reactive variables, meaning that they get updated by any changes
	# To get our result updated by changes in input variable, each analysis on input variables should be in reactive context
	# In this way our calculation/analysis is repeated by any changes on input variables.

	# All render* functions have reactive context

	# We can impose reactive context by 'reactive({})' and 'observe({})'

	# 'reactive({})' is helful when we want to define a new reactive variable
	# 'observe({})' is helpful when we want to observe the result of calculations on a reactive varible
	# Both functions guarantee our working in reactive context.
	# To have access to a reactive variable defined by 'reactive({})' we have to add parantheses after
	# the name of defined variable.


# server <- function(input, output){
# 	library(tidyverse)
# 	library(readr)
# 	(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
# 	output$coolplot <- renderPlot({
# 		bcl %>%
# 			filter(Price >= input$priceInput[1],
# 						 Price <= input$priceInput[2],
# 						 Type == input$typeInput,
# 						 Country == input$CountryInput) %>% 
# 			ggplot(aes(x = Alcohol_Content)) + 
# 			geom_histogram()
# 	})
# 	
# 	output$results <- renderTable({
# 		bcl %>% 
# 			filter(Price >= input$priceInput[[1]],
# 						 Price <= input$priceInput[[2]],
# 						 Type == input$typeInput,
# 						 Country == input$CountryInput)
# 	})
# 	pricediff <- reactive({diff(input$priceInput)})
# 	observe(print(pricediff()))
# 	# avr <- reactive({(pricediff())/2})
# 	# observe({avr()})
# }


# To make the code more simple we define one filtred data as a reactive variable to be used in
# multiple render functions, instead of repeating filtering in each render function.

# uiOutput(): used to render more UIs (seems weired, but it is useful)
	# used to create inputs or any other UIs from the server or in other words dynamically
	# useful when an input is dependant on another input (see file shiny2 )

# Generating selection list for countries throughy 'uiOutput()' imposes some errors at first
# since it needs access to 'input$CountryInput' to generate 'fitered' reactive variable, while 
# it does not exist. Thus some modifications are needed in the 'filtered' section of server function.
# Furthermore 'ggplot()' can not deal with a 'NULL' data set, so renderPlot section of server function
# needs modification as well. However, there are not any issues with 'renderTable' section since shiny
# does not have a problem in rendering a "Null" table

# When using a reactive variable in a reactive context, it is considered as dependency of
# that context, so the reactive context gets re-executed for each update in the reactive variable.
# In the case that suppressing re-execution is desireable, the reactive variable should be used
# inside the 'isolate()' function.

# Server function executes for any user connected to your website
# Due to this, just objects defined or get changed through input variables imposed by user should
# be defined inside the 'server' function.
# Big data sets or functions that should be available globally (functions which are used through
# server function) should be defined in 'app.R' out of 'server' function.
# 'global.R' is useful when we want to define something to be available in both 'ui.R' and 'server.R'

## Useful packages in shiny:
# shinyjs: Easily improve the user interaction and user experience in your Shiny apps in seconds
# shinythemes: Easily alter the appearance of your app
# leaflet: Add interactive maps to your apps
# ggvis: Similar to ggplot2, but the plots are focused on being web-based and are more interactive
# shinydashboard: Gives you tools to create visual “dashboards”

(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
server <- function(input, output){
	library(tidyverse)
	library(readr)
	
	filtered0 <- reactive({
		if ((is.null(input$CountryInput)) & (is.null(input$typeInput))) {
			return (NULL)
		}
		bcl %>% 
			filter(Type == input$typeInput)
	})
	
	filtered <- reactive({
		if (is.null(filtered0())) {
			return (NULL)
		}
		
		if(input$checksubtypeInput == TRUE) {
			if (is.null(input$subtypeInput)){
				return(NULL)
			} else {
				filtered0() %>%
					filter(Price >= input$priceInput[[1]],
								 Price <= input$priceInput[[2]],
								 Country == input$CountryInput,
								 Subtype == input$subtypeInput)
			}
		} else {
			filtered0() %>%
				filter(Price >= input$priceInput[[1]],
							 Price <= input$priceInput[[2]],
							 Country == input$CountryInput)
		}
		
	})
	output$coolplot <-renderPlot({
		if (is.null(filtered())) {
			return()
		}
		ggplot(filtered(),aes(x = Alcohol_Content)) +
			geom_histogram()
	})

	output$results <- renderTable({
		filtered()
	})
	pricediff <- reactive({diff(input$priceInput)})
	observe(print(pricediff()))
	avr <- reactive({(pricediff())/2})
	observe({print(avr())})
	output$CountryOutput <- renderUI({
		selectInput("CountryInput", "Country", choices = sort(unique(bcl$Country)), 
								selected = "CANADA")
	})
	
	output$typeOutput <- renderUI({
		radioButtons(inputId = "typeInput", label = "Product Type",
								 choices = sort(unique(bcl$Type)),
								 selected = "WINE")
	})
	

	output$subtypeOutput <- 
		renderUI({
			if (input$checksubtypeInput == FALSE) {
			return(NULL)
				} else {
			selectInput("subtypeInput", "Product Sub-Type", choices = sort(unique(filtered0()$Subtype)))
		}
		})
	
	observe({
		print(filtered()$Alcohol_Content)
	})
	
	# observe({print(input$checksubtypeInput)})
	# observe({print(filtered0())})
	# observe({print(filtered0())})
	
}


