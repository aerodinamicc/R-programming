#Action 1
library(tidyverse)
cru <- read_csv("E:/R/EnvMon/cru_kg.csv")
data1 %>% filter(year==1979 &month==10) %>%
ggplot() +
  geom_tile(aes(x=lon, y=lat, fill=tmp)) +
  coord_quickmap() # setting up  specific scale so that when we modify the frame, the objects remain with the same shape

glb <- read_csv("E:/R/EnvMon/GLB.Ts+dSST.csv", skip = 1, na = "***")
ggplot(glb, aes(x = year, y = MAM)) + geom_point() + geom_smooth(method = "glm")

#Action 2
ggplot(glb, aes(x = "Jan", y = "April")) +
  geom_point() +
  geom_smooth(method = "lm")

#Action 3
italy <- read_csv("italy.csv")
it <- ggplot(italy,
             aes(x = long, y = lat,
                 group = group)) +
  geom_polygon(fill = "orange", colour = "red")
it
it + coord_map("mercator")
it + coord_map("cylindrical")
it + coord_map("orthographic")#

#Action3

data1 %>% filter(year %in% 1981:2000 & month==3) %>% group_by(lon, lat) %>% summarize(avgt = mean(tmp, na.rm = TRUE))%>%
  mutate(warmEnough = avgt > -2)%>%ggplot(aes(x=lon, y=lat, fill = warmEnough))
names(data1)
