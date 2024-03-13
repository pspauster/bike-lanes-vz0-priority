library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)


bike_lanes_17 <- read_sf("Archived_nyc_bike_routes_2017 2") %>% 
  mutate(length = st_length(geometry))

priority_districts <- read_sf("https://data.cityofnewyork.us/resource/fxnz-5gsp.geojson") %>% 
  st_transform(st_crs(bike_lanes_17))

bike_lanes_23 <- read_sf("https://data.cityofnewyork.us/resource/s5uu-3ajy.geojson?$limit=100000") %>% 
  st_transform(st_crs(bike_lanes_17)) %>% 
  mutate(length = st_length(geometry))

#new_lanes <- st_difference(bike_lanes_23, bike_lanes_17)

crashes_cyclists <- read_csv(URLencode("https://data.cityofnewyork.us/resource/h9gi-nx95.csv?$query=SELECT * WHERE crash_date >= '2017-01-01T00:00:01.000' AND (number_of_cyclist_injured > 0 OR number_of_cyclist_killed >0) LIMIT 100000"))

community_boards <- read_sf("https://data.cityofnewyork.us/resource/jp9i-3b7y.geojson")

cbs_nolanes <- c("303", "314", "317", "312")

priority_cbs <- c("303", "314", "317", "312", "315", "305", "304", "404", "403")

crashes_cyclists_sf <- crashes_cyclists %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(st_crs(community_boards)) %>% 
  mutate(in_cb = lengths(st_intersects(.,community_boards %>% filter(boro_cd %in% cbs_nolanes))),
         in_priority = lengths(st_intersects(.,community_boards %>% filter(boro_cd %in% priority_cbs))))

summary <- crashes_cyclists_sf %>% 
  group_by(in_cb) %>% 
  summarize(total_injuries = sum(number_of_persons_injured, na.rm = T),
            total_killed = sum(number_of_persons_killed, na.rm = T))

summary_priority <- crashes_cyclists_sf %>% 
  group_by(in_cb, in_priority) %>% 
  summarize(total_injuries = sum(number_of_persons_injured, na.rm = T),
            total_killed = sum(number_of_persons_killed, na.rm = T))


bike_lanes_23_clean <- bike_lanes_23 %>%
  st_transform(st_crs(community_boards)) %>% 
  st_intersection(community_boards%>% filter(boro_cd %in% priority_cbs))

bike_lanes_17_clean <- bike_lanes_17%>%
  st_transform(st_crs(community_boards)) %>% 
  st_intersection(community_boards%>% filter(boro_cd %in% priority_cbs))


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    position: fixed !important;
    padding: 10px;
    background: rgba(255,255,255,0.8);
    color: black;
    border-radius: 5px;
  }
  .leaflet .legend i {
    height: 2px;
    vertical-align: middle;
  }
  .title {
    padding: 0px;
    margin: 0px;
    font-size: 20px;
    margin-block: 0px;
    font-weight: 800;
  }
  span {
    font-weight: 800;
    color: red;
  }
  p {
    font-size: 15px;
    margin: 0px;
    padding: 10px;
    margin-block: 0px;
  }
  img {
  max-width: 428px
  }
  @media (max-width:475px) {
  img {max-width:280px}
  .title {font-size: 13px}
  p {font-size: 9px}
  }
  
"))

title <- tags$div(
  tag.map.title, HTML("<img src = https://patrickspauster.com/images/streetsblog-logo.png><p class = 'title'>No New Bike Routes In South Brooklyn</p><p>New Bicycle routes in Priority Community Boards since 2017<br><span>Select a CB</span> to view the number of injuries and deaths</p>")
)  

routes_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  setView(lat = 40.6686969078186, lng = -73.93827935702873, zoom = 12) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(data = community_boards %>% filter(boro_cd %in% priority_cbs),
              color = "red",
              fill = FALSE,
              stroke = TRUE,
              weight = 1,
              popup = ~paste0(boro_cd)) %>% 
  addPolylines(data = bike_lanes_23_clean,
               color = "gold",
               opacity = 1,
               weight = 0.75
               ) %>%
  addPolylines(data = bike_lanes_17_clean,
               color = "green",
               opacity = 1,
               weight = 0.75) %>%
  addControl(title,
             className = "map-title",
             position = "topleft") %>% 
  addLegend("bottomleft",
            colors = c("gold", "green"),
            labels = c("New bike routes", "Existing bike routes")) %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") 

routes_map

mapview::mapshot(routes_map, url = "map.html", file = "map.png")



