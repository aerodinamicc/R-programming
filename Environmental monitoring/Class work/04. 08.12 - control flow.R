#if-else clause controls flow of programme/function by checking conditions:
x <- 11
if (x >= 10) {
  print("x is at least 10")
} else {
  print("x is smaller than 10")
}
#Second example
x <- 1
if (x < 10) {
  x <- 10
}
#What about fried mars bars?
check_breakfast <- function(x) {
  if (x == "fried mars bars") {
    print("yikes!")
  } else {
    print("how about fried mars bars?")
  }
}
check_breakfast("toast")
#ACTION 1
#Write a function that takes a vector as argument and checks for its length.
#If the vector is longer than 3, the function should return only the first 
#three elements of the vector, otherwise, the complete vector.

VecLength <- function(vec){
  if (length(vec) > 3){
    print(vec[1:3])
  } else {
    print(vec)
  }
}
#test vector
c<- c(1,4,5,6,7,8,9)

#for loops
for (i in 1:10) {
  print("Bishops love sci-fi!")
}

for (i in c("b", "c", "d")) {
  print(paste("Give me a", i))
}
#giving back length of list attributes
l <- list(a = 1, b = 2:4, c = letters[1:4])
for (i in 1:length(l)) {
  print(length(l[[i]]))
}

#Good way to create sequences of indices: seq_along():
#Depending on the length of the vector (n), seq_along() creates a vector with 1, 2, 3..., n elements
l <- list(a = 1, b = 2:4, c = letters[1:4])
for (i in seq_along(l)) {
  print(length(l[[i]]))
}
#tidyverse
l <- list(a = 1, b = 2:4, c = letters[1:4])
for (i in seq_along(l)) {
  l[[i]]) %>%
  length()
}

#While loops
z <- 0
while (z < 10) {
  print("z is smaller than 10")
  z <- z + 1 #this code increments 1 to the value of z after eah itiration
}
#To print each container of the list
u <- list(bird = "peacock",
          wood = "doussie",
          friends = c("anne", "claude",
                      "sascha", "maude"))
n <- length(u)
for (i in 1:n) {
  print(i)
  print(u[[i]])
}
#ACTION 2
#A pilgrim approaches his target by going two steps forward and one step back.
#Use a while loop to compute how long it takes him to travel 1000 m when one step
#measures 0.5 m and takes 1 second to complete.

distance <- 0
sec <- 0
while (distance < 1000){
  distance <- distance + 0.5
  sec <- sec + 3
}
sec <- sec - 4
print(paste(sec/3600, " hrs"))

#BETTER
distance <- 0
sec <- 0

while (distance < 1000){
  distance <- distance + 1
  sec <- sec + 2
  if (distance>=1000){break}
  distance <- distance - 0.5
  sec <- sec + 1
}
print(paste(sec/3600, " hrs"))

#More complex functions
foo <- function(x, y) {
  xy_ratio <- x/y
  if (xy_ratio > 1) {
    log(xy_ratio)
  } else {
    sqrt(xy_ratio)
  }
}
foo(2, 3)
#Last evaluated expression is returned
foo <- function(x) {
  if (x > 2) {
    x + 10
  } else {
    return(x - 10) #return() can be useful to make clear that you are returning early (e.g., after error checking).
  }
}
foo(2)
#The readr package
#Great for plain-text rectangular files: csv, tsv, fwf, log-files, .
#read_csv() and read_csv2() for comma/semicolon separated files
#read_tsv() for tab-separated files
#read_delim() for files with any delimiter
#read_table() for columns separated by whitespace
#read_fwf() reads a fixed width file

#Skipping lines
read_*(..., skip = 3)
read_*(..., comment = "!") #to skip the first starting with "!"
#Encoding NA
read_*(..., na = "-9999") #to set the NA encoding for your file (here: -9999).

#readr functions read into tibbles ??? there won't be row names.
#Traditional R functions may create row names for data.frames.

#Column names
#readr functions will try to treat the first row as column names
read_csv("birds,lice\n1,3\n2,9") #birds,lice come up as column names
read_csv("1,3\n2,9") #1 and 3 come up as column names
read_csv("1,3\n2,9", col_names = FALSE) # no column names are assigned

#Assigning/changing column names
read_csv("1,2,3\n4,5,6", col_names = c("A", "B", "C"))
read_csv("A,B,C\n1,2,3\n4,5,6") #A,B,C are taken as column names
read_csv("A,B,C\n1,2,3\n4,5,6", col_names = c("i", "j", "k")) #treats A,B,C as value in the first row


#reading the example data into R
setwd("E:/R/EnvMon/example_data")
x1 <- read_csv("file1.txt", col_names = TRUE)
x2 <- read_delim("file2.txt", "*", na = "NA")
x3 <- read_tsv("file3.txt", na = "*****")

write_csv(x3, "myfile.csv", na = "NA")

guess_parser()#
setwd("E:/R/EnvMon/parsing")
dir()
tmps <- read_csv("tmp.csv", col_types = cols(tmp = col_double(), year = col_character()))

#ACTION 5
#There is another problematic file under S:\\EcoDA\\parsing\\tmp2.txt.
#Here, the problem is again with parsing the tmp column, but this time a
#little different. Can you fix it?

y <- read_tsv("tmp2.txt", col_types=cols(tmp = col_double()))
y <- read_tsv("tmp2.txt",guess_max = 1100)
