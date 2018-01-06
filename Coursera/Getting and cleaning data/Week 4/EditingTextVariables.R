tolower()
toupper()

strsplit()
splitNames <- strsplit(names(Data), "\\.") #\\ escapes the dot
#strsplit returns a list so data should be addressed
splitNames[[6]][1]
firstElement <- function(x){x[1]}
sapply(splitNames, firstElement)
#it would return only the first part of the names (excluding whatever was after the dot)
sub()
sub("_", "", names(whatever))#why the comma behind
gsub() #will replace numerous occurances of a symbol
#Searching for values in variable names
grep("Almeida", cameraData$intersection)#it will take a string and will through a var and return where the string appears
grep("string", Data, value = TRUE) #will return the actual string in which "string" was found
grepl() #will return a logical vector
table(grepl("string", Data)) #will show up how many T\F values are there
#stringr package
library(stringr)
nchar("Z A") #3
substr("Zlatan", 1, 4)
paste("Z","A")
paste0("Z","A") #no space inbetween
str_trim() # trims off any white space at the end of a string
