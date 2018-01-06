http://dev.mysql.com/doc/employee/en/sakila-structure.html
http://dev.mysql.com/doc/refman/5.7/en/installing.html
install.packages("RMySQL")
#dbConnect command connects you with the online database; OPENS A CONNECTION, EACH CONNECTION NEEDS TO HAVE A HANDLE (new var)
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <-dbGetQuery(ucscDb,"show databases;");dbDisconnect(ucscDb) #the last thing disconnects from the server.Returns TRUE for disconnection
#hg19 database
hg19 <- dbConnect(MySQL(), user="genome",db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables) #there are more than 10000 table in that single database
allTables[1:5] #looking up the first 5 table in the hg19 database. They describe components of the human genome
#Getting dimensions of a specific table
dbListFields(hg19,"affyU133Plus2") #what are all the field in the particular table
dbGetQuery(hg19, "select count(*) from affyU133Plus2") #how many row are there in the table
#the above is a very special MySQL command that counts all the records (58463)
affyData <- dbReadTable(hg19, "affyU133Plus2") #Getting the data out of the dataset
head(affyData)
#Subsetting the data
query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3") #"*" represents the observations(in this case - all)
#the previous line havent suck out the data into the computer yet
affyMis <- fetch(query); quantile(affyMis$misMatches) #misMatches is one of the columns in the dataset
affyMisSmall <- fetch(query, n=10); dbClearResult(query);
#n=10, says only the first 10 records #dbClearResult(query) is used to remove the query from the remote server
dim(affyMisSmall)
dbDisconnect(hg19) #closes the connection
# http://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf
# http://pants.org/software/mysql/mysqlcommands.html