#Q1 and Q2
setwd("G:/R/Coursera/GCD/Week 1")
if (!file.exists("Idaho.csv")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileUrl, destfile = "Idaho.csv")
}
ComData <- read_csv("Idaho.csv", col_names = TRUE)
m <- ComData %>% filter(ST == "16", VAL=="24") %>% nrow()

#Q3
library(xlsx)
if (!file.exists("NatGas.xlsx")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
  download.file(fileUrl, destfile = "NatGas.xlsx")
}
#For some reason after the a/m dl the file does not open properly.
#After manual dl it seems to work fine
NatGas <- read.xlsx("NGAP.xlsx", header=TRUE, sheetIndex=1)

#readxl
library(readxl)
#Compared to many of the existing packages (e.g. gdata, xlsx, xlsReadWrite) readxl has no external
#dependencies so it's easy to install and use on all operating systems. It is designed to work with
#tabular data stored in a single sheet.
#Readxl supports both the legacy .xls format and the modern xml-based .xlsx format.
read_excel("NatGas.xlsx")

#XML
library(XML)
filexml <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(filexml, "BaltRestaur.xml")
doc <- xmlTreeParse("BaltRestaur.xml", useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
length(xmlChildren(rootNode))
rootNode[[1]][[1]][[2]]

#Q4 SOLUTION
m <- xpathSApply(rootNode, "//zipcode", xmlValue)
sum(m == "21231")

#More XML functions
xmlSize(rootNode[[1]]) #the same as the next#
length(xmlChildren(rootNode[[1]]))
xmlSize(rootNode[[1]][[1]]) #gives back the number of elements


