library(lubridate)
library(tidyverse)
ymd("2017 Feb 2") + days(0:7)
ymd("2017 Feb 2") %>% day()
ymd("2017 Feb 2") + days(0:9) %>% day()

setwd("E:/R/EnvMon")
glb <- read_csv("GLB.Ts+dSST.csv", skip = 1,na = "***")
glb <- glb %>% 
gather(month, temp, Jan:Dec) %>% 
select(year = Year, month, temp)


lu <- 1:12
names(lu) <- month.abb
glb <- glb %>% 
mutate(month = lu[month])

suns <- read_table("SN_m_tot_V2.0.txt",
                   col_names = c("year", "month", "yr2", "count", "x1", "x2"))
mutate(suns, month = as.integer(month))
#one solution merging
suntemp <- merge(suns, glb) %>% as_tibble()

glb <- glb %>% mutate(date = ymd(paste(year, month, 1))) %>% select (-year, -month)
suns <- suns %>% mutate(date = ymd(paste(year, month, 1))) %>% select(-yr2, -month, -year, x1, x2)
suntemp2 <- merge(glb, suns) %>% as_tibble()

 ggplot(suntemp2) +
  geom_line(aes(x = date,
                y = scale(count))) +
  geom_line(aes(x = date,
                y = scale(temp)),
            colour = "red")
 
#ACTION 2
 suntemp2 %>% select(date, count, temp)
   mutate(count = scale(count), temp = scale(temp)) %>%
   gather(variable, z_score, count:temp) %>% 
   ggplot(aes(x = date,
                 y = z_score,
                 color = variable)) +
   geom_line()
 