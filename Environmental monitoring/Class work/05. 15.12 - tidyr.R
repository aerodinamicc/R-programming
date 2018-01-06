library(tidyverse)
nh_temp <- 
  read_csv("http://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts+dSST.csv",
           skip = 1, na = "***")
#in this case Column headers are values, not variable names
#Multiple variables are stored in one column
library(tidyr)
nh_temp -> gather(nh_temp, key = "Month",
       value = "Temp", Jan:Dec) #'why does this not work'?
nh_temp %>% select(Year, Month, Temp) %>%
  filter(Year >= 1950, Year <= 2010)%>%
  group_by(Month) %>%
  summarise(mean = mean(Temp))

#
monthly_temp <- nh_temp %>%
  gather("Month", "Temp", Jan:Dec) %>%
  filter (Year >= 1950, Year <= 2010) %>% #or filter(Year %in% 1950:2010)
  group_by(Month) %>%
  summarise(mean = mean(Temp)) # -> monthly_temp

month <- 1:12
names(month)<- c("Jan", "Feb", "Mar", "Apr", 
                 "May", "Jun", "Jul", "Aug",
                 "Sep", "Oct", "Nov", "Dec")
month["Feb"]

#lookup table
monthly_temp <- nh_temp %>%
  gather("Month", "Temp", Jan:Dec) %>%
  filter (Year >= 1950, Year <= 2010) %>%
  select (Year, Month, Temp) %>%
  mutate(Month_num = month[Month]) %>%
  filter (Year %in% 1950:2010, Month_num %>% 3:10) %>%
  group_by(Month) %>%
  summarise(meant = mean(Temp))

#Q4
setwd("E:/R/EnvMon/")
messy <- read_csv2("messy.csv", col_names = TRUE)
spread (messy, variable, january)  



