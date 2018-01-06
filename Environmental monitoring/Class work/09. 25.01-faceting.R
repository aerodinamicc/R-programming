library(tidyverse)
library
g <- ggplot(iris, aes(x = Sepal.Length,
                      y = Petal.Length)) +
  geom_point() + geom_smooth(method = "lm")
g + facet_grid(. ~ Species)
g + facet_grid(Species ~ .)
g + facet_grid(Sepal.Length < 6 ~ Species)

#Action 1
cru <- read_csv("E:/R/EnvMon/cru_kg.csv")
g <- cru %>% filter(year == 1980 | year == 1990 | year == 2000) %>%
  filter(cls == "Cfb" | cls =="Cfc"| cls =="Dfb"| cls =="Dfc") %>%
  group_by(year, lon, lat) #%>% summarize(avgt = mean(tmp, na.rm = TRUE))

m <- ggplot(g) +
  geom_tile(aes(x=lon, y=lat, fill=tmp))


m + facet_grid(year ~ cls)

#proffesors solutions
g <- cru %>% filter(year %in% c(1980,1990,2000), cls %in% c("Cfb","Cfc","Dfb","Dfc")) %>% 
  group_by(year, lon, lat) %>% summarize(avgt = mean(tmp, na.rm = TRUE), cls = unique(cls)) # unique is the key, since it keep the variety of cls into play 
  ggplot(g, aes(x = avgt, colour = factor(year))) +
    geom_density() + 
    facet_wrap(year ~ cls)
  
#profs solution 2
  g <- cru %>% filter(year %in% c(1980,1990,2000)) %>% str_detect(cls, "[CD]f[bc]") %>%
    group_by(year, lon, lat) %>% summarize(avgt = mean(tmp, na.rm = TRUE), cls = unique(cls)) 
  ggplot(g, aes(x = avgt, colour = factor(year))) +
    geom_density() + 
    facet_wrap( ~ cls) #if we have year~cls we would and up with 12 boxes




