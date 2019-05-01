library(raster)
img_raster <- raster("C:/Users/dupred/Desktop/11145-data/data/osm.png")

plot(img_raster)

# test <- rasterToPolygons(img_raster)
test <- rasterToPolygons(img_raster, dissolve=TRUE)
test <- rasterToContour(img_raster)

plot(test)

# ------------------------------------------------------------------------------
# https://gis.stackexchange.com/questions/187798/create-polygons-of-the-extents-of-a-given-raster-in-r

img_raster <- raster("C:/Users/dupred/Desktop/11145-data/data/osm.png")

e <- extent(img_raster)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  

# make all values the same. Either do
r <- img_raster > -Inf
# or alternatively
# r <- reclassify(x, cbind(-Inf, Inf, 1))

# convert to polygons (you need to have package 'rgeos' installed for this to work)
pp <- rasterToPolygons(r, dissolve=TRUE)

# look at the results
plot(x)
plot(p, lwd=5, border='red', add=TRUE)
plot(pp, lwd=3, border='blue', add=TRUE)


# ------------------------------------------------------------------------------
library(magick)
img <- image_read("C:/Users/dupred/Desktop/11145-data/data/osm.png")

print(img)

img_raster <- as.raster(img)
img_raster[1000,1000]
plot(img_raster)

tiff_file <- tempfile()
image_write(img, path = tiff_file, format = 'tiff')
r <- raster::brick(tiff_file)
raster::plotRGB(r)
test <- rasterToPolygons(r)

# https://gis.stackexchange.com/questions/166753/fastest-way-to-convert-big-raster-to-polyline-using-r-or-python

library(raster)
library(stars)
library(sf)
library(magrittr)

img_raster <- raster("C:/Users/dupred/Desktop/11145-data/data/osm.png")
plot(img_raster)
img_raster[img_raster[] < 750] <- 0
img_raster[img_raster[] >= 750] <- 1

x <- st_as_stars(img_raster) %>% 
  st_as_sf() %>% # this is the raster to polygons part
  st_cast("MULTILINESTRING") # cast the polygons to polylines

x <- st_as_stars(img_raster) %>% 
  st_as_sf() 

#https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

p <- gdal_polygonizeR(img_raster)





