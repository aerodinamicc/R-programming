#QUIZ 4
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
setwd("G:/R/Coursera/GCD/Week 4")
download.file(fileUrl, destfile = "Idaho.csv")
library(tidyverse)
ComData <- read_csv("Idaho.csv", col_names = TRUE)
n <- strsplit(names(ComData), "wgtp")
n[[123]]
#"" "15"

#Q2

GDP <- read_csv("G:/R/Coursera/GCD/Week 3/GDP.csv", skip = 10, col_names = FALSE)
GDP <- GDP[,c(1,2,4,5)]
names(GDP) <- c("CountryCode", "Rank", "Country", "GDP")
GDP <- GDP[complete.cases(GDP), ]
mean(GDP$X5, na.rm = TRUE)
#377652.4

#Q3

grep("^United",GDP$X4)

#Q4
EduData <- read_csv("G:/R/Coursera/GCD/Week 3/EduData.csv")
glo <- merge(GDP, EduData, by.x = "CountryCode", by.y= "CountryCode", all = TRUE)
table(grepl("^Fiscal year end: June", glo$`Special Notes`))
#13

#Q5
install.packages("quantmod")
library(quantmod)
#the following was provided
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
#
library(lubridate)
#the function
year <- function(x) {as.numeric(format(x,'%Y'))}
years <- sapply(sampleTimes, year)
table(years == 2012) #250 values

days <- sapply(years, wday)
table(days == 2)
#47