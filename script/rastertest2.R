img <- magick::image_read("C:/Users/dupred/Desktop/11145-data/data/osm.png")
print(img)

img_raster <- raster::as.raster(img)
plot(img_raster)

tiff_file <- tempfile()
image_write(img, path = tiff_file, format = 'tiff')
r <- raster::brick(tiff_file)
raster::plotRGB(r)
test <- rasterToPolygons(r)

img_raster[1,1]

img_raster[img_raster[] < 750] <- 0
img_raster[img_raster[] >= 750] <- 1

#https://stackoverflow.com/questions/28859181/how-to-get-contour-lines-around-the-grids-in-r-raster
library(raster)
library(rgeos)  ## For dissolve=TRUE in rasterToPolygons()

## Recreate your data
set.seed(2)
r <- raster(nrow=10000, ncol=10000)
r[] <- runif(ncell(r))   
plot(r)

## Compute and then plot polygons surrounding cells with values greater than 0.6
SP <- rasterToPolygons(clump(r>0.6), dissolve=TRUE)
plot(SP, add=TRUE)

img_raster <- raster("C:/Users/dupred/Desktop/11145-data/data/osm.png")
SP <- rasterToPolygons(clump(img_raster>100), dissolve=TRUE)
test <- clump(img_raster>100)
