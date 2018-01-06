setwd("G:/") #flash drive

#loading in the packages
library(tidyverse)
library(stringr)


testX <- read_table("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)
testY <- scan("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
subjectTest <- scan("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
trainX <- read_table("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
trainY <- scan("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
subjectTrain <- scan("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

#Reading the features.txt so that they could be assigned later as column names
features <- read_table("./R/Coursera/GCD/Assignment/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", col_names = FALSE)
#the following creates a vector out of the dataframe
value <- function(x){x}
ValueVector <- sapply(features,value)

#Merging the data
myData <- bind_rows(trainX, testX) #bind_rows is part of the dplyr package but is the quivalent to rbind()
myData <- cbind(myData, c(trainY, testY))
myData <- cbind(myData, c(subjectTrain, subjectTest))
names(myData) <- c(ValueVector, "Activity", "Subject")

#mutating one more column with the activity class (in words)
activityNum <- 1:6
activityClass <- c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING","LAYING")
myData <- myData %>% mutate(ActClass = activityClass[Activity])

#Select only the variables related to mean and std values
myData <- myData %>% select(contains("mean()"), contains("std()"), Activity:ActClass)

#the following would get rid of the numbers in front of the variables' names
splitVars <- strsplit(names(myData), "^[0-9]+ ")
GetSecond <- function(x){x[2]}
names(myData) <- sapply(splitVars, GetSecond)

#getting rid of the temporary variables
rm(splitVars)

#grouping and summarising
myData1 <- myData %>% group_by(Subject, ActClass) %>% summarise_each(funs(mean), 1:69)

#Output file
write.table(myData1, "angelov_tidy_data_180x69", row.names = FALSE)


