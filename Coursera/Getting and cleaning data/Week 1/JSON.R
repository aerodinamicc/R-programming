#JSON Javascript Object Notation
#structure similar to XML
install.packages("jsonlite", repos="http://cran.r-project.org")
library(jsonlite)
names(jsonData)
setwd("E:/R/Coursera/GCD/Week 1")
download.file("https://api.github.com/users/jtleek/repos", "jsondata")
jsonData <- fromJSON("jsondata")
names(jsonData$owner) #makes clear that there are many elements nested in the owner
jsonData$owner$login #gives back all the login values nested in owner
#exporting data that is going to be used by API could be done with convertion to JSON
myjson <- toJSON(iris, pretty=TRUE) #in this case: data.frame to a JSON
cat(myjson)
#convert json back to object
iris2 <- fromJSON(myjson)
print(iris2)

http://www.json.org
http://www.r-bloggers.com/new-package-jsonlite-a-smarter.json-encoderdecoder/