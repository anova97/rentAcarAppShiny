require(shiny)
require(shinydashboard)
library(shinyjs)
header <- dashboardHeader(title = "Rent a CAR")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(uiOutput("body"))


ui <- dashboardPage(header, sidebar, body)
#useShinyjs()