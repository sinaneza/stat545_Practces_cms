library(shiny)
library(DT)
library(readr)
library(tidyverse)

(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
print(str(bcl))

ui <- fluidPage(
	titlePanel("BC Liquour Store Prices"),
	sidebarLayout(
		sidebarPanel(
			sliderInput(inputId = "priceInput", label = "Price", min = 0, max = 100,
									value = c(25, 40), pre = "$"),
			uiOutput("typeOutput"),
			uiOutput("CountryOutput"),
			checkboxInput("checksubtypeInput", "Separate for subtypes?", value = FALSE),
			uiOutput("subtypeOutput")
		),
		mainPanel(plotOutput(outputId = "coolplot"),
							br(), br(), br(), br(),
							DT::dataTableOutput("results")))
)

server <- function(input, output){
	library(tidyverse)
	library(readr)
	(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
	
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
	
	output$results <- DT::renderDataTable({
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
	
}

shinyApp(ui = ui, server = server)