library(shiny)
library(tm)
library(stringr)
library(doParallel)
registerDoParallel()

source(file.path("models2.R"))

wordCount1 <- readRDS(file.path("data3", "wordCount1.Rds"))
wordCount2 <- readRDS(file.path("data3", "wordCount2.Rds"))
wordCount3 <- readRDS(file.path("data3", "wordCount3.Rds"))
wordCount1NoStem <- readRDS(file.path("data3", "wordCount1NoStem.Rds"))

shinyServer(function(input, output, clientData, session) {
        globalWords <- list()
        getPredictedWord <- function(txt){
                myArr <- FilterInput(txt)
                print("server getPredictedWord func")
                predictArr <- rep("...", 3)
                spellingArr <- rep("", 3)
                
                # get the spelling suggestions
                if(length(myArr) > 0) {
                        suggestedWords <- PredictWord(myArr[length(myArr)], wordCount1NoStem)
                        
                        if(length(suggestedWords) > 0) {
                                for(i in 1:length(suggestedWords)) {
                                        spellingArr[i] = FilterOutput(suggestedWords[i], contractionsDF)
                                }
                        }
                }
                
                ## get the prediction 
                if(length(myArr) > 1) {
                        predicted <- PredictKN3(myArr[1], myArr[2], wordCount1, wordCount2, wordCount3, limit = 3, cutoff = )
                        
                        if(length(predicted) > 0) {
                                for(i in 1:length(predicted)) {
                                        predictArr[i] = FilterOutput(predicted[i], contractionsDF)
                                }
                        }
                }
                else if(length(myArr) == 1) {
                        predicted <- PredictKN2(myArr[1], wordCount1, wordCount2)
                        
                        if(length(predicted) > 0) {
                                for(i in 1:length(predicted)) {
                                        predictArr[i] = FilterOutput(predicted[i], contractionsDF)
                                }
                        } 
                }
                
                return(as.data.frame(list(spelling = spellingArr, predicted = predictArr)))
        }
        
        predictionReactive <- reactive(getPredictedWord(input$text))
        
        getWord1 <- reactive({
                w <- predictionReactive()
                return(w$predicted[1])
        })
        
        getWord2 <- reactive({
                w <- predictionReactive()
                return(w$predicted[2])
        })
        
        getWord3 <- reactive({
                w <- predictionReactive()
                return(w$predicted[3])
        })
        
        getSpelling1 <- reactive({
                w <- predictionReactive()
                return(w$spelling[1])
        })
        
        getSpelling2 <- reactive({
                w <- predictionReactive()
                return(w$spelling[2])
        })
        
        getSpelling3 <- reactive({
                w <- predictionReactive()
                return(w$spelling[3])
        })
        

        observe({
                w1 <- getWord1()
                w2 <- getWord2()
                w3 <- getWord3()
                s1 <- getSpelling1()
                s2 <- getSpelling2()
                s3 <- getSpelling3()
                
                output$choice1 <- renderUI({
                        if(w1 == "..." || is.na(w1)) {
                                actionButton("action1", class="my_predictive_btn", label = w1, disabled= TRUE)
                        }
                        else {
                                actionButton("action1", class="my_predictive_btn", label = w1)
                        }
                })
                
                output$choice2 <- renderUI({
                        if(w2 == "..." || is.na(w2)) {
                                actionButton("action2", class="my_predictive_btn", label = w2, disabled= TRUE)
                        }
                        else {
                                actionButton("action2", class="my_predictive_btn", label = w2)
                        }
                }) 
                
                output$choice3 <- renderUI({
                        if(w3 == "..." || is.na(w3)) {
                                actionButton("action3", class="my_predictive_btn", label =w3, disabled= TRUE)
                        }
                        else {
                                actionButton("action3", class="my_predictive_btn", label = w3)
                        }
                })
                
                output$spell1 <- renderUI({
                        if(s1 == "" || is.na(s1)) {
                                actionButton("spelling1", class="my_spelling_btn", label = s1, disabled= TRUE)
                        }
                        else {
                                actionButton("spelling1", class="my_spelling_btn", label = s1)
                        }
                })
                
                output$spell2 <- renderUI({
                        if(s2 == "" || is.na(s2)) {
                                actionButton("spelling2", class="my_spelling_btn", label = s2, disabled= TRUE)
                        }
                        else {
                                actionButton("spelling2", class="my_spelling_btn", label = s2)
                        }
                }) 
                
                output$spell3 <- renderUI({
                        if(s3 == "" || is.na(s3)) {
                                actionButton("spelling3", class="my_spelling_btn", label = s3, disabled= TRUE)
                        }
                        else {
                                actionButton("spelling3", class="my_spelling_btn", label = s3)
                        }
                })
        })
        
        observeEvent(input$action1, {
                isolate({
                        updateTextInput(session, "text", value = paste(str_trim(input$text), getWord1()))
                })
        })
        
        observeEvent(input$action2, {
                isolate({
                        updateTextInput(session, "text", value = paste(str_trim(input$text), getWord2()))
                })
        })
        
        observeEvent(input$action3, {
                isolate({
                        updateTextInput(session, "text", value = paste(str_trim(input$text),getWord3()))
                })
        })
        
        observeEvent(input$spelling1, {
                isolate({
                        txtVal <- gsub("\\s(\\w+)$", "", str_trim(input$text))
                        txtVal <- paste(txtVal, getSpelling1())
                        updateTextInput(session, "text", value = txtVal)
                })
        })
        
        observeEvent(input$spelling2, {
                isolate({
                        txtVal <- gsub("\\s(\\w+)$", "", str_trim(input$text))
                        txtVal <- paste(txtVal, getSpelling2())
                        updateTextInput(session, "text", value = txtVal)
                })
        })
        
        observeEvent(input$spelling3, {
                isolate({
                        txtVal <- gsub("\\s(\\w+)$", "", str_trim(input$text))
                        txtVal <- paste(txtVal, getSpelling3())
                        updateTextInput(session, "text", value = txtVal)
                })
        })
        observeEvent(input$back, {
                isolate({
                        updateTextInput(session, "text", value = gsub("\\s(\\w+)$", "", input$text))  
                })
        })
})
