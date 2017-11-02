

# Usually the first step is populating our app with some visual elemnts. Adding elememnts to our ui.R
## This function just put strings beside each other in an ugly way.
# ui <- fluidPage("BC Liquor Store", "prices")
# To make it more pretty, we use following functions.
# Shiny html like text functions:
# h1(): head1, equivalent to <h1>
# h2(): head2, equivalent to <h2>
# strong(): to bold, equivalent to <strong>
# em(): To italiacize, equivalent to <em>
# br(): For line break, equivalent to <br>
# img(): To add image
# a(): For a hyper link.
# tag(): To add any arbitrary HTML tag with the same names (creation of functions similar to previous ones)

# Test the following ui and make comparison with the previous one.

# ui <- fluidPage(h1("My App"),
# 								"Bc",
# 								"Liqour", 
# 								br(),
# 								"Store",
# 								strong("prices"),
# 								div("this is blue", style = "color: blue;"))


# titlPanel(): Not only adds the tiltle-like text, but also adds that title to the tab panel of 
# the web page and making it distinctive.

##Adding layout:
# Without layout and with just text and html, everything is stacked plainly over each other
# sidebarLayout(): Provides a simple 2 column structure:
# 1)sidebarPanel(): A smaller side bar, generally used for input cntrols
# 2)mainpanel(): Larger panel in which generally resuls are shown.

# We can also do this with 'fluidRow()' and more control on our design.
# column(): Adds a gridded column to 'fluidRow()', number of grids are defined in 'column()'.
# Number of grids are between 1 to 12. ex:
# fluidRow(
# column(2),
# column(10, img(src = "Emission2.png",
# 							 width = "600", heght = "600"))
# )

## Adding inputs to the UI
# There are multiple types of input to be added to UI
# Each has two arguments: inputId, label.
# 'inputId' defines the name of that input to be referred after.
# 'label' is the text as a description to that input printed in the web page beside that variable.
# 'inputId' should be different for each defined linput; otherwise, shiny wont throw an indicative error for that.
# for 'sliderInput()' we have range if we define a vector as its default value.

## Adding outputs to the UI
# Here we are just defining outputs as a placeholders
# Any R object can be an output, like table, plot, etc.
# Outputs should be constructed in the server code later and we are just defining the place
	# of each output and their ID.
# There are several types of output
# ID should be unique for each output and none of the outputs should have the same ID.
# different types of outpit: plotOutput(), tableOutput(), etc

# uiOutput(): used to render more UIs (seems weired, but it is useful)
	# used to create inputs or any other UIs from the server or in other words dynamically
	# useful when an input is dependant on another input (see file shiny2 )

# Generating selection list for countries throughy 'uiOutput()' imposes some errors at first
# since it needs access to 'input$CountryInput' to generate 'fitered' reactive variable, while 
# it does not exist. Thus some modifications are needed in the 'filtered' section of server function.
# Furthermore 'ggplot()' can not deal with a 'NULL' data set, so renderPlot section of server function
# needs modification as well. However, there are not any issues with 'renderTable' section since shiny
# does not have a problem in rendering a "Null" table 

	# Here, I want to use uiOutput to populate country selector and have all the countries in 
	# dataset in selection list.
(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
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
							tableOutput("results")))
)

