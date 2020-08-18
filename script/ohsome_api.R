
# libraries --------------------------------------------------------------------
library(tidyverse)
library(glue)
library(sf)
library(osmdata)
# api endpoint -----------------------------------------------------------------
boundaries_dublin <- 
  osmdata::opq(bbox = 'Dublin, Ireland') %>%
  osmdata::add_osm_feature(key = 'admin_level', value = '7') %>%
  osmdata::osmdata_sf() %>% 
  osmdata::unique_osmdata()

bb <- st_bbox(boundaries_dublin$osm_multipolygons$geometry)

# api endpoint -----------------------------------------------------------------

# https://api.ohsome.org/v0.9/swagger-ui.html?urls.primaryName=dataExtraction#/dataExtraction/geometry
lon1 <- bb[1]
lat1 <- bb[2]
lon2 <- bb[3]
lat2 <- bb[4]
keys <- "highway"
properties <- "tags"
showMetadata <- "false"
time1 <- "2017-01-01" # default: "2014-01-01"
time2 <- "2018-01-01" # default: "2017-01-01"
types <- "way"
url_base <- glue::glue("https://api.ohsome.org/v0.9/elements/geometry?bboxes={lon1}%2C{lat1}%2C{lon2}%2C{lat2}&keys={keys}&properties={properties}&showMetadata={showMetadata}&time={time1}%2C{time2}&types={types}")

url_res <- RCurl::getURL(url_base)

test <- sf::st_read(url_res, stringsAsFactors = FALSE)

# test -------------------------------------------------------------------------
str(test)
st_crs(test)
plot(test$geometry)

motorway_test <- test %>% 
  dplyr::filter(highway == "motorway")
plot(motorway_test$geometry)
