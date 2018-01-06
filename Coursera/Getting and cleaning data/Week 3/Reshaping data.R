#Start with reshaping

library(reshape2)
head(mtcars)

#then on with the melt function we say which of the vars are ID vars and which are measure vars
#why is the next line important for the running of the subsequent code?
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
#tall skinny dataset

#NEXT - Casting
#The following will now show the number of values found for the mpg and hp fields, per distinct
#value in the cyl field. So when cyl=4, there are 11 mpg values and 11 hp values.

cylData <- dcast(carMelt, cyl~variable)
#cylinders as to the newly created 'variable' column
## Aggregation function missing: defaulting to length

cylData

#we can also pass it a function and it will result in a summary based on (in this case) cylinders
#DCAST - take a melted dataset and turn it into a dataframe
cylData <- dcast(carMelt, cyl~variable, mean)
cylData <- dcast(carMelt, cyl~variable, sum)

#Average(ing) values based on a particular factor
head(InsectSprays)
# I'd like to apply to $count along the index $spray the function sum
tapply(InsectSprays$count,InsectSprays$spray,sum)

##   A   B   C   D   E   F 
## 174 184  25  59  42 200
#http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/

#Another way - split
#take $counts and split it up by $spray

spIns = split(InsectSprays$count,InsectSprays$spray)
#results in a list
spIns
#then what we can to is apply a function to that list; with lapply we can apply across that list
sprCount = lapply(spIns,sum)
sprCount
#To make it result in a vector
unlist(sprCount) #OR
sprCount <- sapply(spIns,sum)
#The .(spray) notation is apparently equivalent to "spray". The above will split, apply and
#combine all in one step.

library(plyr)
ddply(InsectSprays, .(spray),summarise, sum=sum(count))
#1st argm - dataset
#2nd argm - these are the variables that wed like to summarise #.() so that we dont have to use""
#3rd arm  we wanna summarise this variable
#4th argm - how do we summarise it - we do a sum of the count variable

#Create a new variable (adding it to the dataset)
#The result is a list of each spray type, and the number of times each spray type is listed,
#is the number of times it is found in the original data set. The "sum" field for this spray
#type, in each occurrence, will be the sum of all values for that spray type. 

spraySums <- ddply(InsectSprays,.(spray),summarise,sum=ave(count,FUN=sum))
dim(spraySums)


