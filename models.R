PredictKN2 <- function(w, wordCount1, wordCount2, limit = 3, cutoff = 20) {
        # d
        d <- length(wordCount2[wordCount2==1]) / (length(wordCount2[wordCount2==1]) + 2 * length(wordCount2[wordCount2==2]))
        
        # cw1, nw1*, n**
        cw1 <- wordCount1[names(wordCount1) == w]
        
        if(length(cw1) == 0) {
                ## the word doesn't exist
        }
        else {
                # cw1w2, n*w2 
                nw1star <- sum(wordCount2[grepl(paste0("^", w), names(wordCount2))])
                nstarstar <- length(wordCount2)
                
                
                listw1star <- wordCount2[grepl(paste0("^", w, " "), names(wordCount2))]
                ## need to catch listw1star error here
                
                if(length(listw1star) > 0) {
                        if(length(listw1star) > cutoff) {
                                ubound = cutoff
                        }
                        else {
                                ubound = length(listw1star)
                        }
                        
                        pkn <- rep(NA, ubound)
                        for(i in 1:ubound) {
                                w2 <- unlist(strsplit(names(listw1star[i]), " "))[2]
                                
                                #cw1w2, n*w2
                                cw1w2 <- wordCount2[names(wordCount2) == paste0(w, " ", w2)]
                                nstarw2 <-  sum(grepl(paste0(w2,"$"), names(wordCount2)))
                                
                                pkn[i] <- max(cw1w2 - d, 0)/cw1 + d/cw1*nw1star*nstarw2/nstarstar
                                #print(pkn[i])
                        }
                        
                        probList <- data.frame(w = listw1star[1:ubound], probability = pkn, stringsAsFactors = FALSE)
                        probList <- probList[order(probList$probability, decreasing = TRUE), ]
                        
                        return(probList[1:limit,])
                }
                else {
                        print("length = 0")
                }
                
        }
        
}

PredictKN3 <- function(w1, w2, wordCount1, wordCount2, wordCount3, limit = 3, cutoff = 10) {
        #calculate d3, d2
        d3 <- length(wordCount3[wordCount3==1]) / (length(wordCount3[wordCount3==1]) + 2 * length(wordCount3[wordCount3==2]))
        d2 <- length(wordCount2[wordCount2==1]) / (length(wordCount2[wordCount2==1]) + 2 * length(wordCount2[wordCount2==2]))
        
        # nw1w2*, cw1w2
        nw1w2star <- sum(grepl(paste0("^", w1, " ", w2, " "), names(wordCount3)))
        cw1w2 <- wordCount2[names(wordCount2) == paste0(w1, " ", w2)]
        
        if(length(cw1w2) == 0) {
                #bailing out to PKN2
                print("cw1w2 = 0")
                return(PredictKN2(w2, wordCount1, wordCount2, limit, cutoff))
        }
        else {
                #nw2*, cw2
                nw2star <- sum(grepl(paste0("^", w2, " "), names(wordCount2)))
                cw2 <- wordCount1[names(wordCount1) == w2]
                
                ## n**
                #nstarw2 <- sum(grepl(paste0(w2,"$"), names(wordCount2)))
                nstarstar <- length(wordCount2)
                
                ## get a list of w1w2* from trigram
                listw1w2star <- wordCount3[grepl(paste0("^", w1, " ", w2, " "), names(wordCount3))]
                print(length(listw1w2star))
                if(length(listw1w2star) > 0) {
                        if(length(listw1w2star) > cutoff) {
                                ubound = cutoff
                        }
                        else {
                                ubound = length(listw1w2star)
                        }
                        
                        pkn <- rep(NA, ubound)
                        for(i in 1:ubound) {
                                ## get cw1w2w3, n*w3
                                #print("-------------")
                                w3 <- unlist(strsplit(names(listw1w2star[i]), " "))[3]
                                #print(w3)
                                cw1w2w3 <- wordCount3[names(wordCount3) == paste0(w1, " ", w2, " ", w3)]
                                #print(cw1w2w3)
                                
                                nstarw3 <- sum(grepl(paste0(w3,"$"), names(wordCount2)))
                                #print(nstarw3)
                                
                                ## get cw2w3
                                cw2w3 <- sum(grepl(paste0("^", w2, " ", w3, "$"), names(wordCount2)))
                                #print(cw2w3)
                                pw1w2 <- max(cw2w3 - d2,0 )/cw2 + d2/cw2*nw2star*(nstarw3/nstarstar)
                                #print("d2")
                                #print(d2)
                                #print("cw2")
                                #print(cw2)
                                #print("nw2star")
                                #print(nw2star)
                                #print(pw1w2)
                                
                                ## get pkn
                                pkn[i] <- max(cw1w2w3 - d3, 0)/cw1w2 + d3/cw1w2*nw1w2star*pw1w2
                                #print(pkn[i])
                        }
                        #head(pkn, 20)
                        
                        probList <- data.frame(w = listw1w2star[1:ubound], probability = pkn, stringsAsFactors = FALSE)
                        probList <- probList[order(probList$probability, decreasing = TRUE), ]
                        
                        return(probList[1:limit,])
                }
                else {
                        print("length = 0")
                }
                
        }
        
}
