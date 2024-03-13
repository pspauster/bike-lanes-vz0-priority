library(tidyverse)
library(sf)
bike_lanes_17 <- read_sf("Archived_nyc_bike_routes_2017 2")

priority_districts <- read_sf("https://data.cityofnewyork.us/resource/fxnz-5gsp.geojson") %>% 
  st_transform(st_crs(bike_lanes_17))

bike_lanes_23 <- read_sf("https://data.cityofnewyork.us/resource/s5uu-3ajy.geojson?$limit=100000") %>% 
  st_transform(st_crs(bike_lanes_17))

new_lanes <- st_disjoint(bike_lanes_23, bike_lanes_17)

