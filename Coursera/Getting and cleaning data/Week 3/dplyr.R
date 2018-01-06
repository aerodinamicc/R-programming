library(dplyr)

#new stuff
#interesting way to create a new factor variable
mutate(dataset, variable =factor(1 * (tmp>80), labels = c("cold", "hot")))
#tmp>80 will return TRUE (1) or FALSE (0)
#if the expression is evaluated with 0 and since 0 is lower than 1, "cold" is gonna be passed as a label

#then we can group_by and summarise besed on that factor variable
hotcold <- group_by(dataset, variable)
summarize(hotcold, tmp = mean(tmp), o3 = max(o3column))

#theoretically
dataset %>% 
  mutate (variable = factor(1 * (tmp>80), labels = c("cold", "hot")))) %>%
  group_by(variable) %>%
  summarize(variable, tmp = mean(tmp), o3 = max(o3column))

#quick practical example
m <-read.csv("E:/R/Coursera/GCD/Week 1/Idaho.csv")
head(m)

m %>%
  mutate (ser = factor(1 * (SERIALNO<500), labels = c("small", "big"))) %>%
  group_by(ser) %>%
  summarize(region = mean(REGION), elep = max(ELEP))

#mutate a variable based on year
dataset <- mutate(dataset, year = as.POSIXlt(date)$year +1900)