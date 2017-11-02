library(shiny)
ui <- fluidPage(
	tabsetPanel(
		tabPanel("Tab 1", "Hello"),
		tabPanel("Tab 2", "there!")
	)
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)