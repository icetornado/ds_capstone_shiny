
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
                p("This is a demonstration of building a Shiny IO Application."),
                fluidRow(
                        column(8, tags$div(class = "button_line", tags$span("Alternative:", class="line_header"), uiOutput("spell1"), uiOutput("spell2"), uiOutput("spell3")))
                ),
                fluidRow(
                        column(8, tags$textarea(id="text", rows=5, class="form-control"))
                ),
                fluidRow(
                         column(8, actionButton("back", "<< Delete", class="my_back_btn pull-right"))
                ),
                fluidRow(
                        column(8, tags$div(class = "button_line", tags$span("Prediction:", class="line_header"), uiOutput("choice1"), uiOutput("choice2"), uiOutput("choice3")))
                )
        )
)

