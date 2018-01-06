setwd("E:/")
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(lubridate)

#Subsetting the dataframe
MD <- read_csv2("/R/Coursera/EDA/household_power_consumption.txt", col_names = TRUE,
                na = "?", col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                           Time = col_time(format = "%H:%M:%S"),
                                           Global_active_power = "n",
                                           Global_reactive_power = "n",
                                           Voltage = "n",
                                           Global_intensity = "n",
                                           Sub_metering_1 = "n",
                                           Sub_metering_2 = "n",
                                           Sub_metering_3 = "n"))
lower <- as.Date("2007-02-01")
upper <- as.Date("2007-02-02")
MD1 <- MD %>% filter (Date >= lower & Date <= upper)
#write_csv(MD1, "/R/Coursera/EDA/household_power_consumption_short.txt")
x <- strptime(MD1$Time, format = "%H:%M:%S")
rm(lower, upper, MD)
#To get Date and time in the same object
MD1$TimeNew <- as.POSIXct(paste(MD1$Date, MD1$Time), format="%Y-%m-%d %H:%M:%S")

#Plots:
hist(MD1$Global_active_power, main = "Global active power", xlab = "Global Active power (kilowatts)",
     col = "red") # "breaks" could also be an argument
# rug(MD1$Global_active_power)
# abline(v = mean(MD1$Global_active_power), col = "yellow", lwd = 4, lty = 2)

plot(MD1$TimeNew, MD1$Sub_metering_1, type = "l")
lines(MD1$TimeNew, MD1$Sub_metering_2, type = "l", col = "red")
lines(MD1$TimeNew, MD1$Sub_metering_3, type = "l", col =  "blue", xlab = "Energy sub metering")
legend("topright", pch = "1", col = c("black", "red", "blue"),
       legend("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png, file = "plot1.png")
dev.off()


#IN THE VIDEOS:
#Barplot deals with categorical data
barplot(MD1$Global_intensity, col = "wheat", main = "Global intensity")
#not the mos appropriate example since the data is not categorical

#MULTIDIMENTIONAL REPRESENTATION
# boxplot(. ~ .)

#Setting up the number of plots per page
par(mfrow = c(2,2)) # 2by2

#Scatterplot
with(pollution, plot(latitude, pm25, col = var)) # with(), introduces the dataframe
# col = var introduces a different color for each different value in var