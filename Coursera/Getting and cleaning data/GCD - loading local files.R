#Creating a folder
setwd("G:/R/Coursera/GCD")
if (!file.exists("Week 1")){
  dir.create("Week 1")
}
setwd("./Week 1")
if (!file.exists("cameras.csv")){
  fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
  download.file(fileUrl, destfile = "cameras.csv")
  }
dateDownloaded <- date()
cameraDataCsv <- read.table("cameras.csv", header = TRUE, sep ",")
#read.table comes with file, header, sep, quote

#XLSX
library(xlsx)
setwd("G:/R/Coursera/GCD/Week 1")
if (!file.exists("cameras.xlsx")){
  fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
  download.file(fileUrl, destfile = "cameras.xlsx")
}

dateDownloaded <- date()
cameraDataXlsx <- read.xlsx("cameras.xlsx", header=TRUE, sheetIndex=1)
#write.xlsx function
write.xlsx(cameraDataCsv, "camera.xlsx")
#XLConnect package has a lot of option to manipulate xlsx files

#XML
#Most of the scraping is done through XML (widely used in internet applications)
#Components: Markup-labels that give the text structure
#           Content - the actual text of the document
#TAGS - Start tags <section>
#        End tags </section>
#        Empty tag <line-break />
#       <Greeting> Hello, world! </Greeting>
#Attributes are components of the label
#       <img src="jeff,jpg" alt = "instructor"/>
#       <step number ="3"> Connect A to B. </step>
# www.w3schools.com/xml/simple.xml
install.packages("XML")
library(XML)
xmlfile <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(xmlfile, useInternal= TRUE)
rootNode <- xmlRoot(doc) #rootNode wraps the whole document
xmlName(rootNode) #getting the actual name of the xml tree
names(rootNode) #all the nested elements within the rootNode
#ACCESSING the data in the xml file looks like when using a list in R
rootNode[[1]][[1]]
xmlSApply(rootNode, xmlValue) #the latter is the function applied over rootNode elements
#xmlValue in this case gives back the exact value of the element without the tags
#http://stat.berkeley.edu/~statcur/Workshop2/Presentations/XML.pdf