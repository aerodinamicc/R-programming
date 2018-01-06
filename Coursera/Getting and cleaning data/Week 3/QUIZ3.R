IdahoHousing <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(IdahoHousing, "E:/R/Coursera/GCD/Week 3/IdahoHousing.csv")
library(tidyverse)
IH <- read_csv("E:/R/Coursera/GCD/Week 3/IdahoHousing.csv")
IH$AgrLog <- IH$ACR == 3 & IH$AGS==6
which(IH$AgrLog)
IH[IH$AgrLog == TRUE,c("ACR", "AGS", "AgrLog")] #didnt return the expected result

#Q2
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", "E:/R/Coursera/GCD/Week 3/Image.jpg")
library(jpeg)
img <- readJPEG("E:/R/Coursera/GCD/Week 3/Image.jpg", native = TRUE)
#determines the image representation - if FALSE (the default) then the result is an
#array, if TRUE then the result is a native raster representation.
quantile(img, probs = c(0.3, 0.8))

#Q3
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "E:/R/Coursera/GCD/Week 3/GDP.csv")
GDP <- read_csv("E:/R/Coursera/GCD/Week 3/GDP.csv", skip = 10, col_names = FALSE)
GDP <- GDP[,c(1,2,4,5)]
names(GDP) <- c("CountryCode", "Rank", "Country", "GDP")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", "E:/R/Coursera/GCD/Week 3/EduData.csv")
EduData <- read_csv("E:/R/Coursera/GCD/Week 3/EduData.csv")
EduData <- EduData[, c("CountryCode", "Income Group")]
head(GDP)
head(EduData)
glo <- merge(GDP, EduData, by.x = "CountryCode", by.y= "CountryCode", all = TRUE)
glo <- glo[1:238,]
sum(!is.na(glo$GDP))
sum(complete.cases(glo)) # the real deal
glo <- glo[complete.cases(glo),]
glo %>% arrange(desc(Rank))

#Q4
glo %>% group_by(`Income Group`) %>% summarise(oecd = mean(Rank))

#Q5
> quantile(glo$Rank, probs=seq(0, 1, length=6))