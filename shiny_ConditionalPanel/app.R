library(shiny)
ui <- fluidPage(
	numericInput("num", "Number", 5, 1, 10),
	conditionalPanel(
		"input.num >=5",
		"Hello!"
	)
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)