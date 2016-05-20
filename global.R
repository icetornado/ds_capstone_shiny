library(dplyr)
contractionsDF <- read.csv(file.path("data", "contractions.csv"), na.strings = "", stringsAsFactors = FALSE, header = FALSE)
names(contractionsDF) = c("key", "value")
contractionsDF <- mutate(contractionsDF, x = tolower(gsub("'", "", contractionsDF$key)))

#load(file.path("data", "ngrams.RData"))
wordCount1 <- readRDS(file.path("data", "wordCount1.Rds"))
wordCount2 <- readRDS(file.path("data", "wordCount2.Rds"))
wordCount3 <- readRDS(file.path("data", "wordCount3.Rds"))

FilterInput <- function(input) {
        inputDoc <- Corpus(VectorSource(input))
        inputDoc <- tm_map(inputDoc, removePunctuation)
        inputDoc <- tm_map(inputDoc, removeNumbers)
        inputDoc <- tm_map(inputDoc, content_transformer(tolower)) 
        ### remove stop words
        #input <- removeWords(input, stopwords("english"))
        inputDoc <- tm_map(inputDoc, stripWhitespace)
        inputDoc <- tm_map(inputDoc, stemDocument)
        inputDoc <- tm_map(inputDoc, PlainTextDocument)
        
        strArr <- unlist(strsplit(inputDoc[[1]]$content, " "))
        
        return(c(strArr[length(strArr) - 1], strArr[length(strArr)]))
}

FilterOutput <- function(w, contractions) {
        matching <- contractions[grepl(paste0("^", w, "$"), contractions$x),]$value
        
        if(length(matching) > 0) {
                return(matching)  
        }
        else {
                return(w)   
        }
}
