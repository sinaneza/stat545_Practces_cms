library(shiny)
ui <- fluidPage(
	numericInput("num", "Max Slider Value", 5),
	uiOutput("slider")
)

server <- function(input, output){
	output$slider <- renderUI({
		sliderInput("Slider", "Slider", min = 0, max = input$num, value = 0)
	})
}
shinyApp(ui, server)