#1st question
#token -  446d7cc56e66d7d6d95e5e922d2c29fd07f2117d
library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("ZAvalanche",
                   key = "64701587f015cc66be4c",
                   secret = "25653d9bf237091d51deee795fec681766b2c699")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp, cache=FALSE)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
stop_for_status(req)
content(req)

#Hello Shane. Are you receiving a browser request on localhost:1401 to confirm authentication of R with Github?
#Len
# See the section entitled Reading from APIs here: http://rstudio-pubs-static.s3.amazonaws.com/16815_e9607bbe2eb140278f4146a2560d83e7.html
#2ND QUESTION
#2. The sqldf package allows for execution of SQL commands on R data frames.
#We will use the sqldf package to practice the queries we might send with the dbSendQuery command in RMySQL.
#Download the American Community Survey data and load it into an R object called 
AmComSurvey <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
setwd("E:/R/Coursera/GCD/Week 2")
download.file(AmComSurvey, "AmComSurvey.csv")

#Q4
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
nchar(htmlCode[c(10,20,30,100)])
#to get a better structure)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
html <- htmlTreeParse(url, useInternalNodes = TRUE)

#Q5
urlfwf <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(urlfwf, "fwf.for")
n <- read.fwf("fwf.for", c(10,9,4,9,4,9,4,9,4), skip = 4)


