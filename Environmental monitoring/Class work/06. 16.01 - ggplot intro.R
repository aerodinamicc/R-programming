setwd("E:/")
library(tidyverse)
names(iris)
ggplot(iris)+
  geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species)) #the geometric mapping includes the aestetics
#one observation in ggplot is characterized by 2 variables - x and y
#Action 1
names(ToothGrowth)
ggplot(ToothGrowth)+
  geom_point(aes(x = dose, y = len, shape = supp, color = supp))
#ACTION 2
ggplot(iris)+
  geom_point(aes(Sepal.Length, Sepal.Width, alpha = Species, size = Sepal.Width, color = Species))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
co2 <- read_table("ggplot/CO2_OBS_1850-2005.lpl",
                   skip = 4,
                   col_names = c("year", "value"))

ggplot(co2) +
  geom_line(aes(year, value, color = value > 300, linetype = value < 350))

#ACTION 3
#Scenario 2.6 Degrees
sc26 <- read_table("ggplot/CO2_RCP3PD_1850-2500.lpl",
                   skip = 4,
                   col_names = c("year", "value"))
ggplot(sc26) +
  geom_line(aes(year, value, linetype = value > 300))

#Scenari 8.5

sc26 <- read_table("ggplot/CO2_RCP85_1850-2500.lpl",
                   skip = 4,
                   col_names = c("year", "value"))
ggplot(sc26) +
  geom_line(aes(year, value, linetype = value > 500))

#Reading in all the files
setwd("ggplot/")
cn <- c("year", "conc")
rcp2 <- read_table("CO2_RCP3PD_1850-2500.lpl",
                     skip = 4, col_names = cn)
rcp4 <- read_table("CO2_RCP45_1850-2500.lpl",
                     skip = 4, col_names = cn)
rcp6 <- read_table("CO2_RCP6_1850-2500.lpl",
                     skip = 4, col_names = cn)
rcp8 <- read_table("CO2_RCP85_1850-2500.lpl",
                     skip = 4, col_names = cn)
rcp2$rcp <- "rcp2.6" #rcp2.6 %>% mutate(rcp = "RCP2.6)
rcp4$rcp <- "rcp4.5"
rcp6$rcp <- "rcp6.0"
rcp8$rcp <- "rcp8.5"
rcps <- rbind(rcp2, rcp4, rcp6, rcp8)

ggplot(rcps) +
  geom_line(aes(x = year,
                y = conc,
                colour = rcp))

#Second strategy
