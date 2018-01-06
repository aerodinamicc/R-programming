setwd("C:/Users/user/Desktop/R/hospital")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#this way all the content is read as character
ncol(outcome)
nrow(outcome)
#now converting the data into integers and using it to compile a histogram
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
#draskanici
Vec <- c(11, 17, 23)
Names <- c("heart attack", "heart failure", "pneumonia")
Matrix <- data.frame(Vec, 1,3, colnames = Names )
#krai draskanici
#the function
heart 
best <- function(state, outcome){
  setwd("E:/R/Coursera/hospital")
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  causes <- c("heart attack", "heart failure", "pneumonia")
  if (state %in% outcome$State && outcome %in% causes){
    if (outcome==causes[1]){
      save <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      vec <- save[which(!is.na(save) == TRUE)]
      name <- outcome$Hospital.Name[which(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(vec))]
      print(name)
    }
    else { stop("Error")
      
    }
    }
}