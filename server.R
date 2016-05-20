
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)
library(SnowballC)
library(RWeka)
library(doParallel)
library(ggplot2)
library(slam)

registerDoParallel()
source(file.path("models.R"))

shinyServer(function(input, output) {

        output$value <- renderPrint(list.files(file.path("data")))
        
        output$predicted <- reactive({
                myArr <- FilterInput(input$text)
                if(length(myArr) > 1) {
                        predicted <- PredictKN3(myArr[1], myArr[2], wordCount1, wordCount2, wordCount3)
                        print(predicted)
                        return(predicted[1])
                }
        })
})
