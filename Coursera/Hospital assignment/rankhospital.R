setwd("G:/R/Coursera/hospital")
outcomedf <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
causes <- c("heart attack", "heart failure", "pneumonia")
rankhospital <- function(state, outcome, num){
  if (state %in% outcomedf$State == FALSE){
    stop("invalid state")
  } else if (outcome %in% causes == FALSE){
    stop("invalid outcome")
  } else if (state %in% outcomedf$State && outcome %in% causes){
    # the nested if loop starts from here on
    # it deals with the 3 variations of outcome
    # given a specific outcome it points at a specific column of the initial database
    # here something else could be done using the tempdf[order(tempdf[,11], tempdf[,2], na.last = FALSE), ] function
    # then we need to point at the last segment of the column - tempdf[length(tempdf[,11]), tempdf[,11]]
    if (outcome==causes[1]){
      #create a df with onlz the columns that we will need
      tempdf <- outcomedf[which(outcomedf$State == state),c(2,7,11)]
      #assign names to the columns so that we can call them easier from now on
      names(tempdf) <- c("Hospital", "State", "Outcome")
      #create a df with only the complete cases
      clean <- tempdf[complete.cases(tempdf),]
      #orders the rows based on the two arguments. Note that the "-" sign could be used for decreasing order
      clean <- clean[order(clean$Outcome, clean$Hospital),]
      #create a new df that would contain the rows of choice
      df <- clean[num,1]
      #print the name of the first output
      print(df)
    } else if (outcome==causes[2]){
      tempdf <- outcomedf[which(outcomedf$State == state),c(2,7,17)]
      names(tempdf) <- c("Hospital", "State", "Outcome")
      clean <- tempdf[complete.cases(tempdf),]
      clean <- clean[order(clean$Outcome, clean$Hospital),]
      df<- clean[num,1]
      print(df)
    } else if (outcome == causes[3]){
      tempdf <- outcomedf[which(outcomedf$State == state),c(2,7,11)]
      names(tempdf) <- c("Hospital", "State", "Outcome")
      clean <- tempdf[complete.cases(tempdf),]
      clean <- clean[order(clean$Outcome, clean$Hospital),]
      df<- clean[num,1]
      print(df)
    }
  }
}