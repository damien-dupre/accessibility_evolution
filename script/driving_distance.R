
# libraries --------------------------------------------------------------------
library(tidyverse)
library(gmapsdistance)
library(osrm)
library(sp)
library(cartography)

# bing API example -------------------------------------------------------------

from_mat_in <- '50.666,13.795'
to_mat_in <- '47.50827271,16.48046829'
distunit <- 'mi'
bingkey <- config::get(value = "bingkey",file = "C:/Users/dupred/OneDrive/Keys/config.yml")
starturl <- 'http://dev.virtualearth.net/REST/V1/Routes?'
fromtourl <- str_c(starturl,'&wayPoint.1=',from_mat_in, '&wayPoint.2=', to_mat_in, '&distanceUnit=', distunit, '&key=', bingkey)
u1 <- RCurl::getURL(fromtourl)
u2 <- RJSONIO::fromJSON(u1, simplify=FALSE)
travelDistance <- u2$resourceSets[[1]]$resources[[1]]$travelDistance
travelDistance

# google API example -----------------------------------------------------------

results <- gmapsdistance(
  origin = "38.1621328+24.0029257", 
  destination = "37.9908372+23.7383394",
  mode = "walking", 
  key = config::get(value = "google_cloud",file = "C:/Users/dupred/OneDrive/Keys/config.yml")$key)
results

# osm API example -----------------------------------------------------------

route <- osrmRoute(src = c("A", 13.23889, 52.54250),
                   dst = c("B", 13.45363, 52.42926),
                   sp = TRUE, overview = "full")
route$distance

