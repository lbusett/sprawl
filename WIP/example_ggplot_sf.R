load(url('https://files.fm/down.php?i=kew5pxw7&n=loadme.Rdata'))
library(raster)
library(sf)
library(tidyverse)
library(maps)
devtools::install_github("hadley/ggplot2")


#   ____________________________________________________________________________
#   Transform original data in SpatialPointsDataFrame                       ####

coords = data.frame(lat = values(s[[2]]), lon = values(s[[3]]))
spPoints <- SpatialPointsDataFrame(coords, 
                                   data = data.frame(data = values(s[[1]])), 
                                   proj4string = CRS("+init=epsg:4326"))
#   ____________________________________________________________________________
#   Convert the lat-lon coordinates of the poof the points to the original  ###
#   projection of the model (lcc), then convert the points to polygons in lcc
#   projection and convert to an `sf` object to facilitate plotting

orig_grid = spTransform(spPoints, projection(s))
polys = as(SpatialPixelsDataFrame(orig_grid, orig_grid@data, tolerance = 0.149842),"SpatialPolygonsDataFrame")
polys_sf = as(polys, "sf")
points_sf = as(orig_grid, "sf")
#   ____________________________________________________________________________
#   Plot using ggplot - note that now you can reproject on the fly to any    ###
#   projection using `coord_sf`
   
# Plot in original  projection (note that in this case the cells are squared): 

ggplot(polys_sf) +
  geom_sf(aes(fill = data), alpha = 0.4, color = "transparent") + 
  scale_fill_distiller(palette='Spectral') +
  ggtitle("Precipitations") +
  coord_sf(crs = st_crs(projection(s)))+
  theme_bw()

# Plot in WGS84 latlon projection and add borders: 

ggplot(polys_sf) +
  geom_sf(aes(fill = data)) + 
  scale_fill_distiller(palette='Spectral') +
  ggtitle("Precipitations")  +
  borders('world', colour='black')+
  coord_sf(crs = st_crs(3857))
  theme_bw()

