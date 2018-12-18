setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(maptools)
library(ggpubr)
library(ggmap)
library(leaflet)
library(rgdal)
library(raster)
library(sf)
library(stringr)

polys <- read_sf('polys.shp')

# polys %>%
#   filter(date == '20170327') %>%
#   leaflet() %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.7,
#               fillColor = ~colorQuantile("YlOrRd", mean)(mean),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE)) %>%
#   addTiles()

st_crs(polys)

#7, 13, 14, 400
# polys %>%
#   filter(plot_id == 400) %>%
#   ggplot(aes(x = date, y = mean, group = 1)) +
#   geom_line() +
#   theme(axis.text.x.bottom = element_text(angle = 90))

a <- polys

a <- gather(a, key = "stat", value = "val", 1:6)

#remove geometry not to slow the process
a <- as.data.frame(a)
a <- a[,1:4]

a <- spread(a, date, val)

a <- a %>%
  filter(!(stat %in% c("missVal", "pixelCount", "max", "min", "sd"))) %>%
  dplyr::select(-stat) 

# NA values
sapply(a, function(x){sum(is.na(x))})

# 

for(c in 3:ncol(a) - 1)
{
  col1 = colnames(a)[c]
  col2 = colnames(a)[c+1]
  newCol = paste0(col1, " - ", col2)
  a <- a %>%
    mutate(newCol = !!as.name(col1) / !!as.name(col2))
  colnames(a)[ncol(a)] <- newCol
}

a <- a[,c(1, 29:ncol(a))]
a <- a %>%
  mutate(geometry = polys$geometry[1:1252])

st_write(a, 'divisionPolys.shp')

a.colnames <- colnames(a)[c(3:ncol(a)-1)]

a %>%
  filter(plot_id == 13) %>%
  gather(key = "images", value = "mean", a.colnames) %>%
  ggplot(aes(x= images, y = mean, group = 1)) +
  geom_line() +
  theme(axis.text.x.bottom = element_text(angle = 90))
