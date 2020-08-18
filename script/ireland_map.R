# setup
library(raster)
library(sp)
library(osmplotr)
library(osmdata)
library(magrittr)
library(sf)
library(prettymapr)
library(osmplotr)
#######################################################
global <- map_data("world")

world_map +
  xlim(-12,-5) + 
  ylim(50,57)

ggplot() + 
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group),fill = NA, color = "blue") + 
  coord_fixed(xlim = c(-8,-5),  ylim = c(50,57), ratio = 1.3) +
  #coord_fixed(1.3) +
  theme_bw()

ggplot() +
  geom_raster(data = pdata, aes(x = lat, y = lng, fill = price))+
  geom_point(data = data_gps_sample_dublin, aes(x = lat, y = lng))+
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group),fill = NA, color = "blue") + 
  coord_fixed(xlim = c(max(data_gps_sample_dublin$lat),min(data_gps_sample_dublin$lat)),  ylim = c(max(data_gps_sample_dublin$lng),min(data_gps_sample_dublin$lng)), ratio = 1.3) +
  scale_fill_gradientn(colours = gradian_col)

ggplot() +
  geom_raster(data = pdata, aes(x = lat, y = lng, fill = price))+
  geom_point(data = data_gps_sample_dublin, aes(x = lat, y = lng, colour = price))+
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group),fill = NA, color = "blue") + 
  coord_fixed(xlim = c(max(data_gps_sample_dublin$lat),min(data_gps_sample_dublin$lat)),  ylim = c(max(data_gps_sample_dublin$lng),min(data_gps_sample_dublin$lng)), ratio = 1.3) +
  #scale_fill_gradientn(colours = gradian_col) +
  scale_colour_gradientn(colours = gradian_col, trans = "log10") +
  theme_bw()

#######################################################
ireland <- raster::getData("GADM",country="IRL",level=1)
plot(ireland)
#######################################################
dublin_osm <- opq(bbox = c(min(data_gps_sample_dublin$lng), min(data_gps_sample_dublin$lat), max(data_gps_sample_dublin$lng), max(data_gps_sample_dublin$lat))) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'name', value = "Thames", value_exact = FALSE) %>%
  osmdata_sf()
#######################################################
dublin_dat <- searchbbox("dublin,ireland")
dublin_dat <- c(dublin_dat[1], dublin_dat[2], dublin_dat[3], dublin_dat[4])
dublin_dat
names(dublin_dat) <- c("left", "bottom", "right", "top")
class(dublin_dat) <- "bbox"
osmdata <- osmar::get_osm(dublin_dat, source = osmsource_api())

dublin_boundaries <- extract_osm_objects(key = 'boundary', bbox = dublin_dat)

###############################################################
#bbox <- osmdata::getbb("dublin,ireland")
###############################################################
dublin_dat <- matrix(c(
  min(data_gps_sample_dublin$lng), 
  min(data_gps_sample_dublin$lat), 
  max(data_gps_sample_dublin$lng), 
  max(data_gps_sample_dublin$lat)
), nrow = 2, ncol = 2,
dimnames = list(c("x", "y"),
                c("min", "max")))

dublin_bbox <- opq(bbox = dublin_dat) %>%
  add_osm_feature (key = "natural", value = "coastline") %>%
  osmdata_sf(quiet = FALSE)

ggplot() + geom_sf(data=dublin_bbox$osm_lines)
###############################################################

blade <- dublin_bbox$osm_lines %>% st_union %>% st_line_merge

plot(blade)
###############################################################
water <- opq(bbox = dublin_dat) %>%
  add_osm_feature(key = "natural", value = 'waterway') %>%
  osmdata_sf(quiet = FALSE)

