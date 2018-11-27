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

#old----
hostels <- read_csv("hostels/Hostel.csv") %>% dplyr::select(-`X1`)

#Hostels
cities <- hostels %>%
  separate(Distance, c("distance", "from"), "km ") %>%
  mutate(distance = as.numeric(distance)) %>%
  arrange(City, `distance`) %>%
  group_by(City) %>%
  filter(row_number(City) == 2) %>%
  ungroup() %>%
  dplyr::select(City, lon, lat)

#2016 data----
#V 2.2

dir("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/", pattern = ".shp")

bnd <- read_sf("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/polbnda_jpn.shp")
bnd <- bnd %>%
  filter(pop > 0) %>%
  group_by(laa) %>%
  summarise(pop = sum(pop))
rails <- read_sf("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/raill_jpn.shp")
roads <- read_sf("gm-jpn-all_u_2_1/gm-jpn-all_u_2_1/roadl_jpn.shp")
airport <- st_read("gm-jpn-all_u_2_2/gm-jpn-all_u_2_2/airp_jpn.shp")

dir("gm-jpn-lu_u_1_1/gm-jpn-lu_u_1_1/jpn")

#Those classifications are from 2006
#V 1.1
lu <- raster('gm-jpn-lu_u_1_1/gm-jpn-lu_u_1_1/jpn/lu.tif')
lc <- raster('gm-jpn-lc_u_1_1/gm-jpn-lc_u_1_1/jpn/lc.tif')
elev <- raster('gm-jpn-el_u_1_1/gm-jpn-el_u_1_1/jpn/el.tif')

# bnd <- as(bnd, Class = "Spatial")
# roads <- as(roads, Class = "Spatial")

tracks_length <- rails %>%
  mutate(length = st_length(geometry) / 1000) %>% # /1000 goes for kilemeters
  st_join(bnd) %>%
  as.data.frame() %>%
  group_by(laa) %>%
  summarise(tracks_length = sum(length))

roads_length <- roads %>%
  mutate(length = st_length(geometry)/ 1000) %>%
  st_join(bnd) %>%
  as.data.frame() %>%
  group_by(laa) %>%
  summarise(roads_length = sum(length))

bnd_joined <- bnd %>%
  left_join(tracks_length, by = "laa") %>%
  left_join(roads_length, by = "laa") %>%
  mutate(area_sqkm = st_area(geometry) / 100000, # 10 000 goes for sq km
         pop_dens = pop / area_sqkm,
         rails_per_sqkm = tracks_length / area_sqkm,
         roads_per_sqkm = roads_length / area_sqkm,
         road_rail_ratio = round(roads_per_sqkm / rails_per_sqkm, 1))

#Plotting ----
bnd_sp <- as(bnd_joined, "Spatial")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = bnd_sp, 
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~colorQuantile("YlOrRd", roads_per_sqkm)(roads_per_sqkm),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup=as.character(bnd$pop),
              popupOptions = popupOptions(closeOnClick = TRUE))

#Raster cropping----
bnd_joined <- bnd_joined %>%
  mutate(cat10 = NA,
         cat20 = NA,
         cat30 = NA,
         cat40 = NA,
         cat50 = NA,
         cat60 = NA,
         cat70 = NA,
         cat80 = NA,
         cat90 = NA,
         buildup = NA)

for (row in 1:nrow(bnd_joined)) {
  slice <- as(bnd_joined[row,], "Spatial")
  ext_lu <- raster::extract(lu, slice, df = TRUE)
  
  ext_lu <- ext_lu %>%
    dplyr::select(cat = lu, ID) %>%
    group_by(cat) %>%
    summarise(percentage = round(n()/nrow(ext_lu)*100 , 2)) %>%
    mutate(cat = paste0("cat", cat)) %>%
    spread(cat, percentage)
  
  for (cat in names(ext_lu)) {
    bnd_joined[row, cat] <- as.numeric(ext_lu[cat])
  }
  
  ext_lc <- raster::extract(lc, slice, df = TRUE)
  
  ext_lc <- ext_lc %>%
    dplyr::select(cat = lc, ID) %>%
    group_by(cat) %>%
    summarise(percentage = round(n()/nrow(ext_lc)*100 , 2)) %>%
    mutate(cat = paste0("cat", cat)) %>%
    spread(cat, percentage)
  
  if ("cat13" %in% names(ext_lc)) {
    bnd_joined[row, "buildup"] <- as.numeric(ext_lc["cat13"])
  }
}

st_write(bnd_joined, dsn = "extended_bnd.shp")

#bnd_joined %>% gather(category, percentage, c("cat10":"cat90")) %>% filter(!is.na(percentage)) %>% group_by(laa) %>% summarise(n = n())

#Two main ways to describe CRS in R are an epsg code or a proj4string
# definition. Both of these approaches hacrsve advantages and disadvantages.
# An epsg code is usually shorter, and therefore easier to remember.
# The code also refers to only one, well-defined coordinate reference system.
# On the other hand, a proj4string definition allows you more flexibility when
# it comes to specifying different parameters such as the projection type, the
# datum and the ellipsoid.14 This way you can specify many different projections,
# and modify existing ones. This also makes the proj4string approach more
# complicated. epsg points to exactly one particular CRS.

# Other than searching for EPSG codes online, another quick way to find out about
# available CRSs is via the rgdal::make_EPSG() function, which outputs a data
# frame of available projections.

#available CRSs
#crs_data = rgdal::make_EPSG()
#View(crs_data)

#tidyverse-pitfalls vignette
#top_n(n = 3, wt = pop)

#join non spatial datasets to sf objects is the focus of this section
#left_join() and inner_join() - see vignette("two-table")
#What if we only want to keep countries that have a match in the key variable? In that case an inner join can be used:
#world_coffee_inner = inner_join(world, coffee_data)
#We can identify the rows that did not match using the setdiff()

st_intersects(bnd, airport, sparse = FALSE)
#st_disjoint(), which returns only objects that do not spatially relate in any way to the selecting object

#st_within() returns TRUE only for objects that are completely within the selecting object. This applies only to the first object, which is inside the triangular polygon
#st_touches()

#What about features that do not touch, but almost touch the selection object? These can be selected using st_is_within_distance(), which has an additional dist argument.

#zion = st_transform(zion, projection(srtm))

  

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
