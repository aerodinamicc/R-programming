setwd("G:/R/Coursera/hospital")
outcomedf <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
causes <- c("heart attack", "heart failure", "pneumonia")
#points out the respective columns for the three outcomes when looked up in outcomedf
columns <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
rankall <- function(outcome, num){
    if (outcome %in% causes == FALSE){
    stop("invalid outcome")
  } else if (outcome %in% causes){
    #with the last part of the following code the arg of the function is used to point out the right column from outcomedf
    tempdf <- outcomedf[,c(2,7,as.numeric(columns[outcome]))]
    #renaming the columns for better visibility
    names(tempdf) <- c("Hospital", "State", "Outcome")
    #leaves only the complete cases in finaldf
    finaldf <- tempdf[complete.cases(tempdf),]
    #orders the rows based on the two arguments. Note that the "-" sign could be used for decreasing order
    finaldf <- finaldf[order(finaldf$Outcome, finaldf$Hospital),]
    #split
    x <- split(finaldf, finaldf$State)
    # checks whether you've mentioned best/worst/or an integer
    if (num == "best"){
      num <- 1
      
    }else if (num == "worst"){
      num <- nrow(x$num)
    }
        # the upcoming loop creates a df of hospitals ranked at certain position in each state
  }
}