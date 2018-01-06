d <- date()
class(d) #character
d1 <- Sys.Date()
class(d1) #date
format(d1, "%a, %b, %d, %Y")

x <- c("1jan1960", "2mar1920", "4dez1980") #it is also system language dependatn
y <- as.Date(x, "%d%b%Y")

y[3] - y[1]

months() #of a date vector or a single date
julian()
weekdays()

#Lubridate library
library(lubridate); ymd("20140108") #would serve as as.Date
mdy("08/04/2013")
dmy("03-04-2013")
#it also deals wih time
ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03", tz="Pacific/Auckland")
?Sys.timezone
#in the lubridate package
#refering to a weekday would be done like this:
wday("2013-04-15", label=TRUE)
#Ultimately you want your dates and times as class Date ot classes POSIXct, POSIXlt
