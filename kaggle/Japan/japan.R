setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install.packages(c("tidyverse", "ggrepel", "lubridate", "maptools", "ggpubr", "ggmap", "leaflet", "rgdal", "raster"))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(maptools)
library(ggpubr)
library(ggmap)
library(leaflet)
library(rgdal)
library(raster)

hostels <- read_csv("hostels/Hostel.csv") %>% dplyr::select(-`X1`)

#SHP points
setwd("gm-jpn-all_u_2/gm-jpn-all_u_2")
area <- readOGR("airp_jpn.shp")

coor <- coordinates(area)

midlon <- mean(range(coor[,1]))
midlat <- mean(range(coor[,2]))

area.points <- as.data.frame(area)

leaflet() %>%
  addTiles() %>%  
  setView(midlon, midlat, zoom = 4) %>%
  addMarkers(lng=area.points$coords.x1, lat=area.points$coords.x2, popup=area.points$nam)

setwd("../../gm-jpn-lu_u_1_1/gm-jpn-lu_u_1_1/jpn")

#Raster land use
lu <- raster('lu.tif')

plot(lu)

hist(lu,
     breaks=100,
     col="wheat3")

#Transportation 2.2
setwd("../../../gm-jpn-trans_u_2_2/gm-jpn-trans_u_2_2")
dir(pattern = "*.shp")

ferries <- rgdal::readOGR('ferryl_jpn.shp')

ports <- readOGR('portp_jpn.shp')

ports.points <- as.data.frame(ports)

leaflet() %>%
  addTiles() %>%  
  setView(midlon, midlat, zoom = 4) %>%
  addMarkers(lng=ports.points$coords.x1, lat=ports.points$coords.x2, popup=ports.points$nam)

#population
setwd("../../gm-jpn-pop_u_2/gm-jpn-pop_u_2")
dir(pattern =".shp")

buildup <- readOGR("builtupp_jpn.shp") 

#all folder
setwd("../../gm-jpn-all_u_2_2/gm-jpn-all_u_2_2")
dir(pattern = "*shp")

bnd <- readOGR("polbnda_jpn.shp")

leaflet(bnd) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", pop)(pop),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=bnd$pop)

bnd_pop <- bnd[bnd$pop >0,]
bnd_pop$area_sqkm <- area(bnd_pop) / 1000000
crs(x)
bnd_pop$density <- bnd_pop$pop/bnd_pop$area_sqkm
range(bnd_pop$density)
hist(bnd_pop$density, breaks = 500)

dens_pop <- bnd_pop[bnd_pop$density > 1000,]

#Hostels
cities <- hostels %>%
  separate(Distance, c("distance", "from"), "km ") %>%
  mutate(distance = as.numeric(distance)) %>%
  arrange(City, `distance`) %>%
  group_by(City) %>%
  filter(row_number(City) == 2) %>%
  ungroup() %>%
  dplyr::select(City, lon, lat)

#Density plotted together with cities
leaflet() %>%
  addTiles() %>%
  addPolygons(data = dens_pop, color = "#444444", weight = 1,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", density)(density),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(dens_pop$laa, ", ", round(dens_pop$density, 0), " people per square kilometer"),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addMarkers(lng=cities$lon, lat=cities$lat, popup=cities$City)
