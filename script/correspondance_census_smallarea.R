################################################################################
#                        Distance to OSM Features                              #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(leaflet)
library(sf)

# data -------------------------------------------------------------------------
data_dublin_geocoded_clean <- file.path(data_path, "data_dublin_geocoded_clean.rds") %>% 
  readr::read_rds()

# ireland small area -----------------------------------------------------------
# Small_Areas__Generalised_20m__OSi_National_Boundaries.geojson from https://data.gov.ie/dataset/small-areas-generalised-20m-osi-national-statistical-boundaries-2015
ireland_smallarea <- rgdal::readOGR("http://data-osi.opendata.arcgis.com/datasets/68b14cef8cf247b191ee2737e7e6993d_1.geojson")
# visualisation
leaflet::leaflet(ireland_smallarea) %>% 
  addTiles() %>% 
  addPolygons()
# convert to sf crs
ireland_smallarea <- sf::st_as_sf(ireland_smallarea)
sf::st_crs(ireland_smallarea) <- sf::st_crs(4326) # assign crs
ireland_smallarea <- sf::st_transform(ireland_smallarea, crs = 32721) # transform

# identify GEOGID for each property --------------------------------------------
pnts <- data_dublin_geocoded_clean %>% 
  dplyr::select(lat, lng)

data_dublin_geocoded_clean$GEOGID <- apply(pnts, 1, function(row) {  
  # transformation to palnar is required, since sf library assumes planar projection 
  
  points.df <- data.frame(
    'lat' = row[1], 
    'lng' = row[2]
  )
  points.sf <- sf::st_as_sf(points.df, coords = c("lng","lat"))
  sf::st_crs(points.sf) <- sf::st_crs(4326) # assign crs
  points.sf <- sf::st_transform(points.sf, crs = 32721)
  
  ID <- ireland_smallarea[which(sf::st_intersects(points.sf, ireland_smallarea, sparse = FALSE)), ]$GEOGID
  print(ID)
  ID
})
# clean GEOGID -----------------------------------------------------------------
data_dublin_geocoded_clean <- data_dublin_geocoded_clean %>% 
  dplyr::select_if(~sum(!is.na(.)) > 0) %>% 
  dplyr::mutate(GEOG_ID = stringr::str_remove(GEOGID,"[A]"))

# read census data -------------------------------------------------------------
carers <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/carers.csv")
disabilty_age_group <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/disabilty_age_group.csv")
general_health <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/general_health.csv")
housing_occupancy <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/housing_occupancy.csv")
housing_rooms <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/housing_rooms.csv")
housing_tenure <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/housing_tenure.csv")
housing_type <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/housing_type.csv")
population <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/population.csv")
religion <- readr::read_csv("http://airo.maynoothuniversity.ie/files/dDATASTORE/all_island/csv/religion.csv")

census_common_field <- c("GEOG_ID", "AIRO_AI_ID", "ED_Ward", "ED_WARD_ID", "County_UD", "Country")

census_data <- list(
    carers, 
    disabilty_age_group, 
    general_health, 
    housing_occupancy, 
    housing_rooms,
    housing_tenure, 
    housing_type, 
    population,
    religion
  ) %>% 
  purrr::reduce(dplyr::left_join, by = census_common_field)

# remove duplicated GEOG_ID ----------------------------------------------------
list_geoid <- unique(data_dublin_geocoded_clean$GEOG_ID)

census_data <- census_data %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

# join by GEOG_ID present in the data ------------------------------------------
data_dublin_geocoded_clean <- data_dublin_geocoded_clean %>% 
  dplyr::left_join(census_data, by = "GEOG_ID")