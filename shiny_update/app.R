library(shiny)
ui <- fluidPage(
	sliderInput("slider", "Move me", value = 5, 1, 10),
	numericInput("num", "Number", value = 5, 1, 10)
)
server <- function(input, output, session) {
	observe({
		updateNumericInput(session, "num", value = input$slider)
	})
	observe({
		updateSliderInput(session, "slider", value = input$num)
	})
}
shinyApp(ui = ui, server = server)

# Notice to session in server function.
# Its usage is not mandatory; however, it should be used whenever a function using "session"
# is used inside.
# learn more: ?shiny::session