setwd("G:/R/Coursera/hospital")
outcomedf <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
causes <- c("heart attack", "heart failure", "pneumonia")
#points out the respective columns for the three outcomes when looked up in outcomedf
columns <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

best <- function(state, outcome){
  if (state %in% outcomedf$State == FALSE){
    stop("invalid state")
  } else if (outcome %in% causes == FALSE){
    stop("invalid outcome")
  } else if (state %in% outcomedf$State && outcome %in% causes){
    
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    outcome==causes[1]
    #with the last part of the following code the arg of the function is used to point out the right column from outcomedf
    tempdf <- outcomedf[which(outcomedf$State == state),c(2,7,as.numeric(columns[outcome]))]
    #renaming the columns for better visibility
    names(tempdf) <- c("Hospital", "State", "Outcome")
    #leaves only the complete cases in finaldf
    finaldf <- tempdf[complete.cases(tempdf),]
    #orders the rows based on the two arguments. Note that the "-" sign could be used for decreasing order
    finaldf <- finaldf[order(finaldf$Outcome, finaldf$Hospital),]
    #create a new df that would contain the rows of choice
    df <- finaldf[1,1]
    print(df)
    }
  }
