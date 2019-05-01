# Chiang, Y. Y., & Knoblock, C. A. (2009, July). Extracting road vector data from raster maps. In International Workshop on Graphics Recognition (pp. 93-105). Springer, Berlin, Heidelberg.
# Iosifescu, I., Tsorlini, A., & Hurni, L. (2016). Towards a comprehensive methodology for automatic vectorization of raster historical maps. e-Perimetron, 11(2), 57-76.

# Spatial vectorization

# libraries --------------------------------------------------------------------
library(tidyverse)
library(raster)
library(rgeos)
library(magick)
library(colorfindr)
img_link <- "C:/Users/dupred/Desktop/11145-data/data/osm.png"

# find colors ------------------------------------------------------------------

colorfindr::get_colors(
  img = img_link,
  min_share = 0.05
) %>%
  colorfindr::plot_colors(sort = "size")

# raster with {magick} ---------------------------------------------------------

img <- magick::image_read(img_link)
img_raster <- raster::as.raster(img)

str(img_raster)

# evaluate colors --------------------------------------------------------------

color_df <- as.vector(img_raster) %>% 
  as.data.frame()

colnames(color_df) <- "color"

test <- color_df %>% 
  dplyr::group_by(color) %>% 
  dplyr::summarise(n_col = n())

# matrix(v,nrow = 3,ncol = 4)

# replace colors ---------------------------------------------------------------

test <- gsub('#f7f4edff', '#ffd700', img_raster)

plot(test)

# convert to raster brick ------------------------------------------------------

tiff_file <- tempfile()
image_write(img, path = tiff_file, format = 'tiff')
img_raster_brick <- raster::brick(tiff_file)

raster::plotRGB(img_raster_brick)

test <- raster::raster(tiff_file)

plot(test)

# raster with {raster} ---------------------------------------------------------
img_raster_layer <- raster::raster(img_link)

str(img_raster_layer)

plot(img_raster_layer)

# filter value in raster layer -------------------------------------------------
filter_image <- img_raster_layer %in% 150:400

filter_image2 <- mask(img_raster_layer, filter_image, maskvalue = 1)

plot(filter_image2)

# test -------------------------------------------------------------------------
library(raster)
library(spex)
library(sf)
r <- raster(volcano)
r[sample(ncell(r), 3000)] <- NA
b <- brick(r, r*1.5)
psf <- rasterToContour(r)
plot(r)
plot(psf)

data(dem, package = "RQGIS")
cl = rasterToContour(dem)
plot(dem, axes = FALSE)
plot(cl, add = TRUE)
