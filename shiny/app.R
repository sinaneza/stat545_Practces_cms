# UI is the code powering the app
# server is the web page showing the app

# UI is responsible for creating the layout of the app and telling Shiny exactly where things go.
# The server is responsible for the logic of the app;
# Server is set of instructions telling the webpage what to be shown when user interacts with the page

# UI is responsible for creating the controls. They also tell shiny where to place the controls and whre to
# place the plots.
# Server is responsible for creating the actual plot or the data inside the table.

# The original file should be created in the name of "app.R", otherwise R is unable to recognize it.
# Original file should be located in a separate folder, separated from other R objects, unless those
# objects are used through your shiny app.

# Following template is used in shiny apps to create an empty "ui" and an empty "server".
# library(shiny)
# ui <- fluidPage()
# server <- function(input, output) {}
# shinyApp(ui = ui, server = server)

# shiny::runApp('app.R') can also be used to run the app, instead o ron botton on the top.

# It is also possible to use separate "ui.R" and "server.R" files.
# In this case things assigned to "ui" are in "ui.R" file and things assigned to "server" are 
# in "servsr.R" file.
# "app.R" just contains "shinyApp()" line
library(shiny)
library(readr)
library(tidyverse)
(bcl <- read_csv(paste0(getwd(),"/bcl-data.csv")))
# (bcl <- read_csv(paste0(getwd(), "/shiny","/bcl-data.csv")))
print(str(bcl))


shinyApp(ui = ui, server = server)

