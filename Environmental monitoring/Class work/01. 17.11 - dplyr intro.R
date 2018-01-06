#Climate zones
zones <- read_csv("G:/R/EnvMon/ZonAnn.Ts.csv", na = "*****")
#filter() allows you to subset observations based on their values:
#first argument is a data.frame or a tibble
filter(zones, Year >= 1960) # years after 1960
filter(zones, Glob > 0) # Global anomaly greater than 0
#comparison operators >, <, >=, <=, =, !=, %in% #xor function - TRUE if only one is TRUE  
filter(zones, NHem > Glob) # NHem anomaly was greater than the global mean
filter(zones, `44S-24S` < 0) # when `44S-24S` anomaly was lower than 0
#ACTION 1
zones %>% select (Year, Glob, `EQU-24N`,`24S-EQU`) %>% filter(`EQU-24N`>`24S-EQU`)
#arrange
arrange(zones, Glob)
#descending order
zones %>% arrange(desc(Glob))
#Use more arguments to arrange() to specify tie-breakers
#ACTION 2
zones %>% select(Year, Glob, `64N-90N`) %>% arrange(desc(`64N-90N`))
#the select function
select(zones, Year, Glob, `64N-90N`)
select(zones, Year:`64N-90N`)
select(zones, -SHem) #excluding variables
#'contains(): select variables containing string
select(zones, Year, contains("-"))
#or starts_with(), ends_with(): select variables starting or ending with string
zones %>% rename(year = Year, TAnomaly = Glob)
#ACTION 3
zones %>% select (Year, Arctic=`64N-90N`, Antarctic=`90S-64S`)%>% summary()
#mutate
#ACTION 4
#Create a new logical variable indicating globally warmer than average conditions.
zones %>% mutate (Warmer = Glob>0) %>% summary
#Use summarise() (or summarize()) to collapse a data.frame into a single row:
summarise(zones, max_anom = max(Glob))
#GROUPING
#Group-wise summaries

toy <- tibble(
  year = c(1962, 1963, 1973, 1974, 1981,
           1982, 1990, 1991, 2008, 2009),
  temp = c(0.02, 0.02, 0.22, -0.04, 0.44,
           0.14, 0.54, 0.53, 0.65, 0.79),
  volcano = rep(c("no_eruption", "eruption"), 5)
)

toy_group <- group_by(toy, volcano)

summarise(toy_group, mean_anom = mean(temp))




#act1 - Compute the average July temperature for the Cfb climate.
cli %>% filter(month == 7, cls == "Cfb") %>% summarise(temp = mean(tmp))
#act1.2 - Compute the average July temperatures for all different climate classes.
cli %>% filter(month == 7) %>% group_by(cls) %>% summarise(temp = mean(tmp))

# create groups based on time slots
temp_time <- mutate(zones, after_1960 = Year > 1960) #Creates a new T/F column
temp_time_group <- group_by(temp_time, after_1960) #1st argm is source, 2nd - variable/column
summarise(temp_time_group, max_anom = max(Glob)) # this one summarises the Glob variable based on the groups
#SUMMARISE points fist at the grouped data (1st argm) and then at the variable which is to be summarised in the framework of the groups
#Pipes restructure the calling sequence from nested to linear
temp_mutate <- zones %>% 
  filter(Year > 1980) %>% 
  select(Year:SHem) %>% 
  mutate(hemisph_diff = NHem - SHem)

#ACTION 5
zones%>%
  filter(Year >= 1950) %>%
  select(Year, contains("Hem"))%>% #here it could have been mutate(after_1980, Year >1980)%>% group_by(year_1980)
  group_by(Year>1980)%>%
  mutate(nh_diff = NHem - SHem) %>%
  summarise(mean(nh_diff))

  #CHALLANGE OF THE WEEK
library(tidyverse)

# This challenge is about climate data in monthly resolution. We get different
# months, and also seasonal means, like "MAM" for spring (March, April, May).

# 1. We can also read in data directy from URLs like so:

nh_temp <- read_csv("G:/R/EnvMon/NH.Ts+dSST.csv", skip = 1)

# Unfortunately, this does not work out of the box, since the first line of this
# data set contains meta information that does not match the general data
# structure of the file. Try to adapt the function call to `read_csv` to read in
# the file properly. Also make sure that NAs are correctly parsed!

# 2. Use dplyr verbs to find out which season experienced the strongest warming
# when you compare the periods from 1960 to 1980 and 1981 to 2015!
nh_means <- nh_temp %>% 
  filter(Year %in% 1960:2015) %>% 
  select(Year, DJF:SON) %>% 
  mutate(period = Year < 1981) %>% 
  group_by(period) %>% 
  summarise(anom_winter = mean(DJF),
            anom_spring = mean(MAM),
            anom_summer = mean(JJA),
            anom_fall = mean(SON))

nh_means[1,] - nh_means[2,]

# -> strongest warming in winter


