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
library(sf)
library(stringr)

#Load data----
#2016 data
#V 2.2
#BND stands for the administrative boundaries of the 1692 wards in Japan
bnd <- read_sf("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/polbnda_jpn.shp")

prefectures <- bnd %>%
  filter(pop > 0) %>%
  group_by(nam) %>%
  summarise(pop = sum(pop))

bnd <- bnd %>%
  filter(pop > 0) %>%
  group_by(laa) %>%
  summarise(pop = sum(pop))

rails <- read_sf("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/raill_jpn.shp")
roads <- read_sf("gm-jpn-all_u_2_1/gm-jpn-all_u_2_1/roadl_jpn.shp")
airport <- read_sf("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/airp_jpn.shp")

#Those classifications are from 2006
#V 1.1
lu <- raster('gm-jpn-lu_u_1_1/gm-jpn-lu_u_1_1/jpn/lu.tif')
lc <- raster('gm-jpn-lc_u_1_1/gm-jpn-lc_u_1_1/jpn/lc.tif')
elev <- raster('gm-jpn-el_u_1_1/gm-jpn-el_u_1_1/jpn/el.tif')

#Data prep----

tracks_length <- rails %>%
  mutate(length = as.numeric(st_length(geometry)) / 1000) %>% # /1000 goes for kilemeters
  st_join(bnd) %>%
  as.data.frame() %>%
  group_by(laa) %>%
  summarise(tracks_length = sum(length))

roads_length <- roads %>%
  mutate(length = as.numeric(st_length(geometry)) / 1000) %>%
  st_join(bnd) %>%
  as.data.frame() %>%
  group_by(laa) %>%
  summarise(roads_length = sum(length))

bnd_joined <- bnd %>%
  left_join(tracks_length, by = "laa") %>%
  left_join(roads_length, by = "laa") %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1000000, # 1 000 000 goes for sq km
         pop_dens = pop / area_sqkm,
         rails_per_sqkm = tracks_length / area_sqkm,
         roads_per_sqkm = roads_length / area_sqkm,
         road_rail_ratio = round(roads_per_sqkm / rails_per_sqkm, 1))

#Raster cropping aka LULC addition----
bnd_joined <- bnd_joined %>%
  mutate(forest = NA,
         mixture = NA,
         grassland = NA,
         ariculture = NA,
         wetland = NA,
         barren = NA,
         buildup = NA,
         water = NA,
         ocean = NA)

categories <- tibble()

for (row in 1:nrow(bnd_joined)) {
  slice <- as(bnd_joined[row,], "Spatial")
  ext_lu <- raster::extract(lu, slice, df = TRUE)
  
  ext_lu <- ext_lu %>%
    dplyr::select(cat = lu, ID) %>%
    group_by(cat) %>%
    summarise(percentage = round(n()/nrow(ext_lu)*100 , 2)) %>%
    complete(cat = seq(10, 90, 10)) %>%
    spread(cat, percentage)
  
  categories <- categories %>%
    rbind(ext_lu)
}

bnd_joined[, 10:18] <- categories[,]

#st_write(bnd_joined, dsn = "extended_bnd.shp", quiet = FALSE, factorsAsCharacter = TRUE)

#bnd_joined <- read_sf("extended_bnd.shp")

#All good but the classification does not come in very handy
nrow(bnd_joined %>% filter(is.na(buildup)))

#Where are the cities----
nrow(bnd_joined %>% filter(buildup > 10))
hist(bnd_joined$buildup)

spacedNumbers <- function(x)
{
  if (x > 999999) {
    millions <- x %/% 1000000
    return(paste(millions, x%/%1000 - millions * 1000, x %% 1000, sep = " "))
  }
  else if(x > 999){
    return(paste(x%/%1000, x %% 1000, sep = " "))
  }
  else{
    return(as.character(x))
  }
}

bnd_joined[,"laa" == "Koganei Shi"]

#Plotting ----

#Population leaflet----
leaflet(bnd) %>%
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("YlOrRd", pop)(pop),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=paste(spacedNumbers(bnd$pop), bnd$laa, sep = ", "),
              popupOptions = popupOptions(closeOnClick = TRUE))

#Green municipalities ---- 
bnd_joined <- bnd_joined %>%
  mutate(natural = ifelse(is.na(agriculture) & is.na(buildup), 100,
                          ifelse(is.na(agriculture), 100 - buildup, 
                                 ifelse(is.na(buildup), 0,  100))))

leaflet(bnd_joined) %>%
  addTiles() %>% #urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png"
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("RdYlGn", natural)(natural),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=paste0(bnd_joined$laa,
                           ", ",
                           as.character(bnd_joined$natural),
                           "% natural LU classes"),
              popupOptions = popupOptions(closeOnClick = TRUE))

#Transport hubs----
bnd_joined <- bnd_joined %>%
  mutate(transportation = ifelse(is.na(rails_per_sqkm) & is.na(roads_per_sqkm), 0,
                                 ifelse(is.na(rails_per_sqkm), roads_per_sqkm, rails_per_sqkm)))

leaflet(bnd_joined) %>%
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("YlGnBu", transportation)(transportation),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=paste0(bnd_joined$laa,
                           ", ",
                           as.character(bnd_joined$transportation),
                           " km of roads and rails per sq km"),
              popupOptions = popupOptions(closeOnClick = TRUE))

#Urban density in municipalities with more than 10% build-up area----
urban <- bnd_joined %>%
  mutate(urban_sqkm = round(area_sqkm * buildup / 100, 2), #already in sqkm
         urban_density = round(pop / urban_sqkm, 0)) %>%
  filter(buildup > 20)

binpal <- colorBin("OrRd", urban$urban_density, 6, pretty = TRUE)

leaflet() %>%
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png",
           options = providerTileOptions(minZoom = 5, maxZoom = 10)) %>%
  addPolygons(data = urban, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~binpal(urban_density),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=paste(as.character(urban$urban_density), urban$laa, sep = ", "),
              popupOptions = popupOptions(closeOnClick = TRUE))
			  


#Earthquakes----
eq_input <- read_csv("Japan earthquakes 2001 - 2018.csv") %>%
  mutate(date = ymd_hms(time),
         year = year(time),
         month = month(time),
         day = day(time)) %>%
  dplyr::select(date, year, month, day, longitude, latitude, mag, depth) %>%
  arrange(longitude, latitude)

#High magnitude quakes
eq_highMag <- eq_input %>%
  filter(mag > 7)

### 200km Buffer around Japan
japan <- st_union(bnd)
japan_buffer <- st_buffer(japan, 100000)

#Project all the earthquakes
eq_input <- st_as_sf(x = eq_input, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84") #


#WGS84
prefectures <- st_transform(prefectures, 4326)

eq_pref <- eq_input  %>%
  st_join(prefectures, join = st_intersects) %>%
  filter(!is.na(nam))
  
eq_pref_df <- eq_pref %>%
  group_by(nam) %>%
  summarise(earthquakes_count = n(),
            max_mag = max(mag)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

prefectures <- prefectures %>%
  left_join(eq_pref_df, by = "nam")

eq_pref %>% group_by(nam) %>%
  ggplot(aes(x = nam, y = depth)) +
  geom_boxplot() +
  theme(axis.text.x.bottom = element_text(angle = 90))

#Plot quakes ----
leaflet(prefectures) %>%
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("YlOrRd", max_mag)(max_mag),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=paste(as.character(prefectures$max_mag), prefectures$nam, sep = ", "),
              popupOptions = popupOptions(closeOnClick = TRUE))

#Plot high magnitude quakes
leaflet(prefectures) %>%
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png") %>%
  addMarkers(lng = eq_highMag$longitude, lat = eq_highMag$latitude,
             popup = paste(eq_highMag$date, ", ", eq_highMag$mag, " magnitude, ", eq_highMag$depth, " km depth"))

#EQ on Japanese land since 2001
sum(eq_sf$earthquakes_count)

sp1 <- SpatialPoints(eq)
sf1 <- st_as_sf(sp1)

eq_sf <- bnd %>%
  left_join(sp1) %>%
  group_by(laa) %>%
  summarise(eqs = n())

leaflet(sp1) %>%
  addMarkers(lng = eq$longitude, lat = eq$latitude)


  

#Build up area files are no good

leaflet() %>%
  addTiles() %>% #urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png"
  addPolylines(data = roads) %>%
  addPolygons(data = bnd, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("YlOrRd", pop)(pop),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=as.character(bnd$pop),
              popupOptions = popupOptions(closeOnClick = TRUE))

int = gIntersects(roads, bnd)
