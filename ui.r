library(shiny)
library(shinydashboard)
library(ggplot2)

# general elements of the ui

shinyUI( 
  dashboardPage(
    dashboardHeader( title=textOutput("title")),
    dashboardSidebar(uiOutput("side")),
    dashboardBody(
      uiOutput("page"),
      uiOutput("error")
    )
  )
  
)