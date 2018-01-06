#Webscraping - programatically extracting data from the HTML code of websites
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode
install.packages("XML")
library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url,useInternalNodes = TRUE)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue) #didnt work
htmlNames(html)
#GET from the httr package
library(httr);html2= GET (url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//tittle", xmlValue)
#Accessing websites with passwords using HTTR
pg2 = GET("http://httpbin.org/basic-auth/user/passwd", authenticate("user", "passwd"))
pg2
names(pg2)
#If handles are used, we can save the authentification process #We can easily access the same page on numerous occasions
google=handle("http://google.com")
pg1 =GET(handle=google, path='/') #go get that handle for a specific path
pg2 =GET(handle=google, path="search")
#Once you authenticate urself the cookies will stay and u wont have to do it again
# http://cran.r-project.org/web/packages/httr/httr.pdf
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
