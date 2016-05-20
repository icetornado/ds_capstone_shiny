
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(htmltools)
library(shiny)

shinyUI(
        fluidPage(
                tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = file.path("bootstrap.css")),
                        tags$link(rel = "stylesheet", type = "text/css", href = file.path("project.css"))
                ),
                titlePanel("Capstone Course Project - Text Preditive App"),
                h4("Trieu Tran"),
                h5("May 20, 2016"),
                hr(),
                fluidRow(
                        column(12, 
                               wellPanel(
                                        p("This is a demonstration of building a Shiny IO Application."),
                                        wellPanel(
                                                textInput("text", label = h3("Text input"), 
                                                          value = "Enter text..."),
                                                hr(),
                                                p("Current Value:", style = "color:#888888;"), 
                                                fluidRow(column(3, verbatimTextOutput("value"))),
                                                fluidRow(column(3, verbatimTextOutput("predicted")))
                                        )
                                   
                               )
                        )
                )
        )
)

