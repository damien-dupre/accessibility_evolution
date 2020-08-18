
# libraries --------------------------------------------------------------------
library(plyr)
library(osmdata)
library(sp)
library(sf)
library(tidyverse)
library(jsonlite)
library(leaflet)

# options ----------------------------------------------------------------------
# https://wiki.openstreetmap.org/wiki/Overpass_API
# osmdata::get_overpass_url()
osmdata::set_overpass_url('https://overpass-api.de/api/interpreter')

# sources ----------------------------------------------------------------------
source(here::here("script/functions.R"))

# data path --------------------------------------------------------------------
data_path <- here::here() %>% 
  dirname() %>% 
  dirname() %>% 
  paste0("/data/property_price_ireland_data")

# data wrangling ---------------------------------------------------------------
data_raw <- file.path(data_path, "ppr_all.csv") %>% 
  readr::read_csv() %>%
  dplyr::rename(
    date_of_sale = `Date of Sale (dd/mm/yyyy)`,
    price = `Price (<U+0080>)`
  ) %>%
  dplyr::mutate(price = readr::parse_number(price)) %>%
  dplyr::mutate(date_of_sale = as.Date(date_of_sale, format = "%d/%m/%Y")) %>%
  dplyr::mutate(full_address = paste(Address, County, "Ireland")) %>%
  tibble::rowid_to_column("sale_id") %>%
  dplyr::mutate(year = lubridate::year(date_of_sale))

data_dublin <- data_raw %>%
  dplyr::filter(County == "Dublin") %>% head()

list_address <- data_dublin$full_address

# list_geocodes <- NULL
# 
# for (i in list_address) {
#   print(i)
#   res <- nominatim_osm(i)
#   list_geocodes <- rbind(list_geocodes, res)
# }

list_geocodes <- plyr::ldply(list_address, function(address){
  return(nominatim_osm(address))
})

data_dublin_geocoded <- dplyr::bind_cols(data_dublin, list_geocodes) %>%
  dplyr::filter(!is.na(lat))
# write_rds(data_dublin_geocoded,"data/data_dublin_geocoded.rds") # geocoded address

# addresses to be geocoded -----------------------------------------------------
# data_dublin_to_be_geocoded <- dplyr::anti_join(data_dublin,data_dublin_geocoded)
# write_rds(data_dublin_to_be_geocoded,"data/data_dublin_to_be_geocoded.rds")
# data_dublin_to_be_geocoded <- data_dublin_to_be_geocoded %>%
#   tidyr::separate(Address, sep= ",", into = c("street", NA,NA), remove = TRUE) %>%
#   dplyr::mutate(address_2 = paste(street, County, sep = ", "))
# list_address <- data_dublin_to_be_geocoded$address_2
# list_geocodes <- plyr::ldply(list_address, function(address){
#   return(nominatim_osm(address))
# })
# data_dublin_to_be_geocoded <- readr::read_rds(here::here("data/data_dublin_to_be_geocoded.rds"))
# data_dublin_to_be_geocoded <- data_dublin_to_be_geocoded %>%
#   tidyr::separate(Address, sep= ",", into = c("street", NA,NA), remove = TRUE) %>%
#   dplyr::mutate(address_2 = paste(street, County, sep = ", "))
# 
# list_address <- data_dublin_to_be_geocoded$address_2
# 
# list_geocodes <- plyr::ldply(list_address, function(address){
#   return(nominatim_osm(address))
# })
# osm geocoding api ------------------------------------------------------------
data_dublin <- data_raw %>%
  dplyr::filter(County == "Dublin") %>%
  tidyr::separate(
    Address,
    sep = ",",
    into = c("street", NA, NA),
    remove = TRUE
  ) %>%
  dplyr::mutate(address_2 = paste(street, County, sep = ", "))

list_address <- data_dublin$address_2

list_geocodes <- NULL

# nb_fail <- 0
# 
# repeat{
#   
for (i in list_address) {
  print(i)
  res <- nominatim_osm(i)
  Sys.sleep(1.1)
  
  # if(res$status == "FAILED"){
  #   nb_fail <- nb_fail +1
  # } else {
  #   nb_fail <- 0
  # }
  list_geocodes <- rbind(list_geocodes, res)
  # if(nb_fail > 10) {
  #   write_rds(list_geocodes,paste0("list_geocodes",format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),".rds"))
  #   list_address <- tail(list_address, n=length(list_address)-nrow(list_geocodes))
  #   break
  # }
}
#   
#   if(nrow(data_dublin) == nrow(list_geocodes)){
#     break()
#   }
# }

# gam soap model ---------------------------------------------------------------
data_dublin_geocoded_clean <- file.path(data_path, "data_dublin_geocoded_clean.rds") %>% 
  readr::read_rds()

test <- data_dublin_geocoded_clean %>%
  dplyr::filter(between(year, 2011, 2012)) %>%
  dplyr::mutate(
    year = as.factor(as.character(year)),
    mean_dat = mean(price),
    sd_dat = sd(price)
  ) %>% 
  dplyr::filter(between(price, mean_dat - sd_dat, mean_dat + sd_dat)) %>% 
  dplyr::select(-mean_dat, -sd_dat)

gam_dublin_gps <- 
  mgcv::gam(
    price ~ s(lng, lat, bs = "so", xt = list(bnd = bound), by = year) + year, 
    data = test, 
    method = "REML", 
    knots = knots,
    control = list(trace = TRUE)
  )

summary(gam_dublin_gps)

grid.lat <- seq(from = bb[2],to = bb[4],by = 0.005)
grid.lng <- seq(from = bb[1],to = bb[3],by = 0.005)

grid.year <- c(2011, 2012)

grid.map <- expand.grid(lat = grid.lat, lng = grid.lng, year = grid.year) %>%
  dplyr::mutate(year = as.factor(as.character(year)))

pdata <- transform(grid.map, price = predict(gam_dublin_gps, grid.map, type = "response"))

pdata %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(m_year = mean(price, na.rm = TRUE))

gam.check(gam_dublin_gps)

plot(gam_dublin_gps, pages = 1)

summary(gam_dublin_gps)
vis.gam(gam_dublin_gps, plot.type = "contour", too.far = 0.5, pages = 1)

ggplot() +
  geom_raster(data = pdata, aes(x = lng, y = lat, fill = price)) +
  #geom_point(data = test, aes(x = lng, y = lat), size = 0.1) +
  geom_sf(data = multipol[[1]][[2]], fill = "blue") +
  scale_fill_gradientn("Predicted Price", colours = gradian_col) +
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") +
  facet_wrap( ~ year) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 8))

test %>%
  ggplot(aes(x = price, y = year)) +
  ggridges::geom_density_ridges_gradient(aes(fill = ..density..), size = 0.1) +
  scale_x_continuous("Price") +
  scale_y_discrete("") +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 8)) +
  scale_fill_gradientn(name = "Density", colours = colorRampPalette(c("white", blues9))(256))

gam_dublin_gps <-
  mgcv::gam(
    price ~ s(lng, lat, bs = "so", xt = list(bnd = bound), by = year) + year,
    data = test,
    method = "REML",
    knots = knots,
    control = list(trace = TRUE),
    family = gaussian(link = "identity")
  )

gam.check(gam_dublin_gps)

gam_dublin_gps <- 
  mgcv::gam(
    price ~ s(lng, lat,bs = "so", xt = list(bnd = bound), by = year) + year, 
    data = test,
    method = "REML",
    knots = knots,
    control = list(trace = TRUE),
    family = gaussian(link = "identity")
  )

################################################################################
# osmdata::add_osm_feature(key = 'building', value = 'transportation') = 29%
# osmdata::add_osm_feature(key = 'landuse', value = 'commercial') = 27.2%
variable_osm <- osmdata::opq('Dublin, Ireland') %>%
  osmdata::add_osm_feature(key = 'leisure', value = 'park')

variable_osm %>% 
  osmdata::osmdata_sp() %>% 
  magrittr::use_series(osm_points) %>% 
  sp::plot()

variable_sf <- variable_osm %>% 
  osmdata::osmdata_sf() %>%
  magrittr::use_series(osm_points) %>% 
  magrittr::use_series(geometry) %>% 
  sf::st_sf()

st_crs(variable_sf) <- st_crs(4326) # assign crs
variable_sf <- st_transform(variable_sf, crs = 32721) # transform
variable_sf <- st_combine(variable_sf)

plot(variable_sf)

# distance
data_dublin_geocoded_clean$variable <- as.numeric(st_distance(x = data_sf, y = variable_sf))
data_dublin_2018$variable <- as.numeric(st_distance(x = data_sf, y = variable_sf))
data_dublin_2018$variable2 <- as.numeric(st_distance(x = data_sf, y = variable_sf, by_element = TRUE)) # by_element = TRUE returns the shorter distance

gam_dublin_gps <- mgcv::gam(
  price ~ s(lng, lat, bs = "so", xt = list(bnd = bound)) + s(variable), 
  data = subset(data_dublin_geocoded_clean, year == 2010), 
  method = "REML", 
  knots = knots,
  #control=list(trace = TRUE),
  control = gam.control(nthreads = 3),
  family=gaussian(link = "identity"))

summary(gam_dublin_gps)
################################################################################
list_osm_features <- readr::read_rds(here::here("data/list_osm_features.rds")) %>% 
  dplyr::filter(Value != "*") %>% 
  dplyr::filter(!stringr::str_detect(Value, "/|[(]|User Defined|Number|Date|Name|see opening_hours|[:digit:]")) %>% 
  dplyr::filter(!stringr::str_detect(Key, ":"))

# test <- head(list_osm_features)
# test_data <- head(data_dublin_geocoded_clean)

# convert data to sf class
data_dublin_2018 <- data_dublin_geocoded_clean %>% 
  dplyr::filter(year == 2018)

data_sf <- st_as_sf(data_dublin_2018, coords = c("lng", "lat"))
st_crs(data_sf) <- st_crs(4326) # assign crs
data_sf <- st_transform(data_sf, crs = 32721) # transform

for (i in 1:nrow(list_osm_features)) {
  Key <- list_osm_features[i, "Key"]
  Value <- list_osm_features[i, "Value"]
  variable_name <- paste(Key, Value, sep = "_")
  
  paste(i, variable_name) %>% print()
  # obtain gps coordinates of geographic variables (e.g., cycle lanes)
  
  variable_osm <- osmdata::opq('Dublin, Ireland') %>%
    osmdata::add_osm_feature(key = Key, value = Value)
  
  # variable_osm %>% 
  #   osmdata::osmdata_sp() %>% 
  #   magrittr::use_series(osm_points) %>% 
  #   sp::plot()
  
  variable_sf <- variable_osm %>% 
    osmdata::osmdata_sf() %>%
    magrittr::use_series(osm_lines) %>% 
    magrittr::use_series(geometry) %>% 
    st_sf()
  
  if (nrow(variable_sf) == 0) {
    
    data_dublin_2018 <- data_dublin_2018 %>% 
      dplyr::mutate(!!variable_name := NA)
    
  } else {
    
    st_crs(variable_sf) <- st_crs(4326) # assign crs
    variable_sf <- st_transform(variable_sf, crs = 32721) # transform
    variable_sf <- st_combine(variable_sf)
    
    # distance
    data_dublin_2018 <- data_dublin_2018 %>% 
      dplyr::mutate(!!variable_name := as.numeric(st_distance(x = data_sf, y = variable_sf)))
    
  }
  Sys.sleep(5)
}

test2 <- test_data %>% select_if(~mean(is.na(.)) < 0.5)



ireland_smallarea <- rgdal::readOGR("C:/Users/dupred/Desktop/Small_Areas__Generalised_20m__OSi_National_Boundaries.geojson")

leaflet::leaflet(ireland_smallarea) %>% 
  addTiles() %>% 
  addPolygons()

################################################################################
poly_1 <- ireland_smallarea@polygons[[1]]

leaflet::leaflet(poly_1) %>% 
  addTiles() %>% 
  addPolygons() 
################################################################################
poly_3 <- ireland_smallarea@polygons[[3]]

leaflet::leaflet(poly_3) %>% 
  addTiles() %>% 
  addPolygons()
################################################################################
test <- ireland_smallarea@data




for (i in 1:nrow(list_osm_features)) {
  Key <- list_osm_features[i, "Key"]
  Value <- list_osm_features[i, "Value"]
  variable_name <- paste(Key, Value, sep = "_")
  
  paste(i, variable_name) %>% print()
  # obtain gps coordinates of geographic variables (e.g., cycle lanes)
  
  try(
    data_dublin_2018 <- data_dublin_2018 %>% 
      dplyr::mutate(!!variable_name := osm_feature_dist(Key, Value))
  )
  
  Sys.sleep(5)
  
}
###############################################################################

test_gps <- data_gps_sample_dublin_clean[1,]
point <- c(test_gps$lat,test_gps$lng)
pt1 = st_point(point)

#st_crs(pt1) <- st_crs(4326) # assign crs
################################################################################
#poly_3 <- ireland_smallarea@polygons[[3]]

polygon <- st_as_sf(ireland_smallarea)

test <- polygon$geometry[1]

intersection <- st_intersection(x = test, y = point)
################################################################################
################################################################################
points.df <- data.frame(
  'lat' = test_gps$lat,
  'lng' = test_gps$lng
)
points.sf <- st_as_sf(points.df, coords = c("lat", "lng"))

st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform
################################################################################
polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon <- st_transform(polygon, crs = 32721) # transform

#distances <- st_intersection(y = points.sf, x = polygon)
################################################################################
################################################################################
#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
polygon_GEOGID1 <- polygon[polygon$GEOGID == "A017001001",]
polygon_GEOGID1 <- st_transform(polygon_GEOGID1, 32721)

st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)
################################################################################
################################################################################
#true
points.df <- data.frame(
  'lat' = 52.733128, 
  'lng' = -6.915340
)
#false
points.df <- data.frame(
  'lat' = 53.350140, 
  'lng' = -6.266155
)
points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform

st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)

polygon_GEOGID1[which(st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)), ]$GEOGID
################################################################################
################################################################################
#true
points.df <- data.frame(
  'lat' = 52.733128, 
  'lng' = -6.915340
)
points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform

polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

polygon_ireland[which(st_intersects(points.sf, polygon_ireland, sparse = FALSE)), ]$GEOGID
################################################################################
################################################################################
polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

pnts <- data_gps_sample_dublin_clean %>% 
  dplyr::select(lat,lng)

pnts$GEOGID <- apply(pnts, 1, function(row) {  
  # transformation to palnar is required, since sf library assumes planar projection 
  
  points.df <- data.frame(
    'lat' = row[1], 
    'lng' = row[2]
  )
  points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
  st_crs(points.sf) <- st_crs(4326) # assign crs
  points.sf <- st_transform(points.sf, crs = 32721)
  
  ID <- polygon_ireland[which(st_intersects(points.sf, polygon_ireland, sparse = FALSE)), ]$GEOGID
  print(ID)
  ID
})
data_dublin_2018_osm_features <-
  readr::read_rds(here::here("data/data_dublin_2018_osm_features.rds")) %>%
  dplyr::mutate(year_fact = as.factor(as.character(year)))
#
ireland_smallarea <- rgdal::readOGR(here::here("data/Small_Areas__Generalised_20m__OSi_National_Boundaries.geojson"))

polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

pnts <- data_dublin_2018_osm_features %>% 
  dplyr::select(lat,lng)

data_dublin_2018_osm_features$GEOGID <- apply(pnts, 1, function(row) {  
  # transformation to palnar is required, since sf library assumes planar projection 
  
  points.df <- data.frame(
    'lat' = row[1], 
    'lng' = row[2]
  )
  points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
  st_crs(points.sf) <- st_crs(4326) # assign crs
  points.sf <- st_transform(points.sf, crs = 32721)
  
  ID <- polygon_ireland[which(st_intersects(points.sf, polygon_ireland, sparse = FALSE)), ]$GEOGID
  print(ID)
  ID
})
write_rds(data_dublin_2018_osm_features,"data/data_dublin_2018_osm_features.rds")
data_dublin_2018_osm_features <- read_rds(here::here("data/data_dublin_2018_osm_features.rds")) %>% 
  dplyr::select_if(~sum(!is.na(.)) > 0) %>% 
  dplyr::mutate(GEOG_ID = stringr::str_remove(GEOGID,"[A]"))

list_geoid <- unique(data_dublin_2018_osm_features$GEOG_ID)
################################################################################
carers <- read_csv(here::here("data/aero_data/carers.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

disabilty_age_group <- read_csv(here::here("data/aero_data/disabilty_age_group.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

general_health <- read_csv(here::here("data/aero_data/general_health.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_occupancy <- read_csv(here::here("data/aero_data/housing_occupancy.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_rooms <- read_csv(here::here("data/aero_data/housing_rooms.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_tenure <- read_csv(here::here("data/aero_data/housing_tenure.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_type <- read_csv(here::here("data/aero_data/housing_type.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

population <- read_csv(here::here("data/aero_data/population.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

religion <- read_csv(here::here("data/aero_data/religion.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)
################################################################################
data_dublin_2018_osm_features_aero <- data_dublin_2018_osm_features %>% 
  dplyr::left_join(carers, by = "GEOG_ID") %>% 
  dplyr::left_join(disabilty_age_group, by = "GEOG_ID") %>% 
  dplyr::left_join(general_health, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_occupancy, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_rooms, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_tenure, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_type, by = "GEOG_ID") %>% 
  dplyr::left_join(population, by = "GEOG_ID") %>% 
  dplyr::left_join(religion, by = "GEOG_ID")
################################################################################
amenity <- c(
  "amenity_bar",
  "amenity_college",                             
  "amenity_school",
  "amenity_university",                          
  "amenity_bicycle_parking",
  "amenity_fuel",                               
  "amenity_parking",
  "amenity_community_centre",                    
  "amenity_bench",
  "amenity_embassy",                            
  "amenity_police",
  "amenity_prison",                             
  "amenity_recycling"
)
barrier <- c(
  "barrier_city_wall",                           
  "barrier_ditch",                              
  "barrier_fence",                               
  "barrier_guard_rail",                         
  "barrier_hedge",                              
  "barrier_kerb",                               
  "barrier_retaining_wall",                      
  "barrier_wall",                               
  "barrier_block",                               
  "barrier_bollard",                            
  "barrier_chain",                               
  "barrier_full-height_turnstile",              
  "barrier_gate",                                
  "barrier_jersey_barrier",                     
  "barrier_yes"
)
boundary <- c(
  "boundary_administrative",                    
  "boundary_historic",                           
  "boundary_political",                         
  "boundary_postal_code",                        
  "boundary_protected_area"
)
building <- c(
  "building_apartments",                         
  "building_house",                             
  "building_residential",                        
  "building_commercial",                        
  "building_industrial",                         
  "building_retail",                           
  "building_hospital",                           
  "building_university",                        
  "building_yes"
)
highway <- c(
  "highway_motorway",                           
  "highway_trunk",                              
  "highway_secondary",                          
  "highway_tertiary",                            
  "highway_unclassified",                       
  "highway_residential",                         
  "highway_service",                            
  "highway_motorway_link",                       
  "highway_trunk_link",                         
  "highway_secondary_link",                      
  "highway_tertiary_link",                      
  "highway_pedestrian",                          
  "highway_track",                              
  "highway_road",                                
  "highway_footway",                            
  "highway_steps",                               
  "highway_path",                               
  "highway_cycleway"
)
cycleway <- c(
  "cycleway_lane",                              
  "cycleway_opposite",                          
  "cycleway_opposite_lane",                     
  "cycleway_track",                              
  "cycleway_share_busway",                      
  "cycleway_shared_lane"
)
busway <- c(
  "busway_lane"
)
highway <- c(
  "highway_proposed",                            
  "highway_construction" 
)
junction <- c(
  "junction_roundabout"
)
historic <- c(
  "historic_yes"
)
landuse <- c(
  "landuse_commercial",                          
  "landuse_construction",                       
  "landuse_industrial" ,                         
  "landuse_residential",                        
  "landuse_retail",                              
  "landuse_farmland",                           
  "landuse_grass",                               
  "landuse_military",                           
  "landuse_railway",                             
  "landuse_recreation_ground",                  
  "landuse_religious"
)
leisure <- c(
  "leisure_nature_reserve",                     
  "leisure_park",                                
  "leisure_slipway",                            
  "leisure_sports_centre",                      
  "leisure_stadium",                            
  "leisure_track"
)
man_made <- c(
  "man_made_breakwater",                        
  "man_made_crane",                              
  "man_made_embankment",                        
  "man_made_groyne",                            
  "man_made_pier",                              
  "man_made_pipeline"
)
natural <- c(
  "natural_wood",                               
  "natural_tree_row",                            
  "natural_scrub",                             
  "natural_grassland",                           
  "natural_water",                              
  "natural_beach",                               
  "natural_coastline",                          
  "natural_ridge",                               
  "natural_cliff"
)

place <- c(
  "place_district",                              
  "place_county",                               
  "place_city",                                  
  "place_suburb",                               
  "place_island",                                
  "place_locality"
)
power <- c(
  "power_cable",
  "power_line",
  "power_minor_line",                           
  "power_portal" 
)
line <- c(
  "line_busbar"
)

public_transport <- c(
  "public_transport_platform",                 
  "public_transport_stop_area"                  
)
railway <- c(
  "railway_abandoned",                          
  "railway_disused",                             
  "railway_rail",                               
  "railway_tram",
  "railway_platform"
)
bridge <- c(
  "bridge_yes"
)

cutting <- c(
  "cutting_yes"                                 
)
electrified_contact <- c(
  "electrified_contact_line"                   
)
embankment <- c(
  "embankment_yes"                              
)
service <- c(
  "service_crossover",                          
  "service_siding",                              
  "service_spur",
  "service_yard"                                
)

tunnel <- c(
  "tunnel_yes"
)
usage <- c(
  "usage_main"                                  
)

route <- c(                       
  "route_bicycle",                               
  "route_bus",                                  
  "route_ferry",                                 
  "route_hiking",                               
  "route_power",                                 
  "route_road",                                 
  "route_train",                                 
  "route_tram"
)                                 
shop <- c(  
  "shop_paint",                                  
  "shop_kitchen"                              
)
sport <- c(  
  "sport_badminton",                             
  "sport_equestrian",                           
  "sport_gaelic_games",                          
  "sport_rugby_union",                          
  "sport_running"
)
tourism <- c(
  "tourism_artwork",                            
  "tourism_zoo"                                 
)

waterway <- c(
  "waterway_river",                             
  "waterway_riverbank",                          
  "waterway_stream",                            
  "waterway_canal",                              
  "waterway_drain",                            
  "waterway_ditch",                              
  "waterway_weir",                              
  "waterway_lock_gate"
)

source <- c(
  "source_survey"                              
)
area <- c(
  "area_yes"
)
covered <- c(
  "covered_yes" 
)
disused <- c(
  "disused_yes"
)
tidal <- c(
  "tidal_yes" 
)
carers <- c(              
  "% Provides No Care",                          
  "% 1-19 hours unpaid PW",                     
  "% 20-49 hours unpaid PW",                     
  "% 50+ hours unpaid PW",                      
  "% Total Care Providers"                      
)
disabilty_age_group <- c(
  "% Persons with a disability aged 0-14",      
  "% Persons with a disability aged 15 - 44",    
  "% Persons with a disability aged 25 - 44",   
  "% Persons with a disability aged 45 - 64",    
  "% Persons with a disability aged 65 Plus",   
  "% Total persons with a disability" 
)

general_health <- c(
  "% Very Good",                                
  "% Good",                                     
  "% Fair",                                     
  "% Bad",                                       
  "% Very Bad"
)

housing_occupancy <- c(
  "% Occupied/HS With ususal Residents",         
  "%  Unoccupied/HS Without Ususal Residents" 
)
housing_rooms <- c(
  "% 1 Room (Households)",                      
  "% 2 Rooms (Households)",                      
  "% 3 Rooms (Households)",                     
  "% 4 Rooms (Households)",                      
  "% 5 Rooms (Households)",                     
  "% 6 Rooms (Households)",                      
  "% 7 Rooms (Households)",                     
  "% 8 or more Rooms (Households)",              
  "% Total (Households).x"
)
housing_tenure <- c(
  "% Owner Occupier with Mortgage (Households)",
  "% Owner Occupier No Mortgage (Households)",   
  "% Private Rented",                           
  "% Social Rented",                             
  "% Rented Free of Rent (Households)",        
  "% Total (Households).y"  
)                               
housing_type <- c(
  "% House/Bungalow",                           
  "% Flat/Apartment/BedSit",                     
  "% Caravan/Mobile home/Temperory" 
)               
population <- c(
  "PC_MALE",                                     
  "PC_FEMALE",                                  
  "PCAGE014T",                                   
  "PCAGE1524T",                                 
  "PCAGE2544T",                                  
  "PCAGE4564",                                  
  "PCAEG65P",                                    
  "PCAGE1564",                                  
  "PCAGE80P"
)

religion <- c(
  "PC_RO_CATH",                                 
  "PC_OTH_C",                                   
  "PC_NC_OTH",                                  
  "PC_NS",                                       
  "PC_NO_REL"
)
################################################################################
tbl_y <- data_dublin_2018_osm_features_aero %>%
  dplyr::select(price) %>% 
  as.matrix()
tbl_x <- data_dublin_2018_osm_features_aero %>%
  dplyr::select(lng,
                lat,
                amenity,
                barrier,
                boundary,
                building,
                highway,
                cycleway,
                busway,
                highway,
                junction,
                historic,
                landuse,
                leisure,
                man_made,
                natural,
                place,
                power,
                line,
                public_transport,
                railway,
                bridge,
                cutting,
                electrified_contact,
                embankment,
                service,
                tunnel,
                usage,
                route,                       
                shop,  
                tourism,
                waterway,
                source,
                area,
                covered,
                disused,
                tidal,
                carers,              
                disabilty_age_group,
                general_health,
                housing_occupancy,
                housing_rooms,
                housing_tenure,
                housing_type,
                population,
                religion) %>% 
  as.matrix()
#
xgmodel <- xgboost::xgboost(
  data = tbl_x,
  label = tbl_y,
  objective = "reg:linear",
  eval_metric = "rmse",
  #booster = "gblinear",
  verbose = 1,
  nround = 200
)
pred <- predict(xgmodel, tbl_x)

result <- data.frame(pred = pred, raw = tbl_y)

lm_result <- lm(pred ~ price,result)

summary(lm_result)

importance_matrix <- xgb.importance(model = xgmodel)

carers <- read_csv(here::here("data/aero_data/carers.csv"))

disabilty_age_group <- read_csv(here::here("data/aero_data/disabilty_age_group.csv"))

general_health <- read_csv(here::here("data/aero_data/general_health.csv"))

housing_occupancy <- read_csv(here::here("data/aero_data/housing_occupancy.csv"))

housing_rooms <- read_csv(here::here("data/aero_data/housing_rooms.csv"))

housing_tenure <- read_csv(here::here("data/aero_data/housing_tenure.csv"))

housing_type <- read_csv(here::here("data/aero_data/housing_type.csv"))

population <- read_csv(here::here("data/aero_data/population.csv"))

religion <- read_csv(here::here("data/aero_data/religion.csv"))

census_data <- carers %>% 
  dplyr::left_join(disabilty_age_group) %>% 
  dplyr::left_join(general_health) %>% 
  dplyr::left_join(housing_occupancy) %>% 
  dplyr::left_join(housing_rooms) %>% 
  dplyr::left_join(housing_tenure) %>% 
  dplyr::left_join(housing_type) %>% 
  dplyr::left_join(population) %>% 
  dplyr::left_join(religion)

write.csv(census_data,"data/census_data.csv",row.names = FALSE)
