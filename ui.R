
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
                p("This is a Shiny Application for the Data Science Capstone Project.  
                  It demonstrates the text predictive functionality of the project via a web interface."),
                p("Users can start by typing in the text input area below. Along
                with the users' typing, up to 3 choices will be displayed for the next suggested word.
                In addition, up to 3 word subtitution choices will be displayed for the last word typed.
                Users can select one of these word buttons to clicking on them."),
                fluidRow(
                        column(8, tags$div(class = "button_line", tags$span("Subtitute:", class="line_header"), uiOutput("spell1"), uiOutput("spell2"), uiOutput("spell3")))
                ),
                fluidRow(
                        column(8, tags$textarea(id="text", rows=5, class="form-control"))
                ),
                fluidRow(
                         column(8, actionButton("back", "<< Delete", class="my_back_btn pull-right"))
                ),
                fluidRow(
                        column(8, tags$div(class = "button_line", tags$span("Suggestion:", class="line_header"), uiOutput("choice1"), uiOutput("choice2"), uiOutput("choice3")))
                )
        )
)

