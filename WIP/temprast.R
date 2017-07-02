library(sp)
library(raster)
library(rgeos)
library(data.table)
library(gdalUtils)

# create example raster
r <- raster(nrows=10, ncol=15, xmn=0, ymn=0)
values(r) <- sample(x=1:1000, size=150)

# create example (Spatial) Polygons --> Note that I changed it slightly
# to have a SpatialPolygonsDataFrame with IDs for the different polys

p1 <- Polygons(list(Polygon(coords=matrix(c(50, 100, 100, 50, 50, 15, 15, 35, 35, 15), nrow=5, ncol=2), hole=FALSE)), "1")
p2 <- Polygons(list(Polygon(coords=matrix(c(77, 123, 111, 77, 43, 57, 66, 43),         nrow=4, ncol=2), hole=FALSE)), "2")
p3 <- Polygons(list(Polygon(coords=matrix(c(110, 125, 125, 110, 67, 75, 80, 67),       nrow=4, ncol=2), hole=FALSE)), "3")
lots.of.polygons <- SpatialPolygons(list(p1, p2, p3), 1:3)
lots.of.polygons <- SpatialPolygonsDataFrame(lots.of.polygons, data = data.frame (id = c(1,2,3)))
crs(lots.of.polygons) <- crs(r) # copy crs from raster to polygons (please ignore any potential problems related to projections etc. for now)

# plot both
plot(r) #values in this raster for illustration purposes only
plot(lots.of.polygons, add = TRUE)


# Create a spatial grid dataframe and convert it to a "raster fishnet"
# Consider also that creating a SpatialGridDataFrame could be faster
# than using "rasterToPolygons" in your original approach !
cs <- res(r) # cell size.
cc <- c(extent(r)@xmin,extent(r)@ymin) + (cs/2)   # corner of the grid.
cd <- ceiling(c(((extent(r)@xmax - extent(r)@xmin)/cs[1]), # construct grid topology
                ((extent(r)@ymax - extent(r)@ymin)/cs[2]))) - 1
grd    <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)   # Define grd characteristics
#transform to spatial grid dataframe. each cell has a sequential numeric id
sp_grd <- SpatialGridDataFrame(grd,
                               data = data.frame(id = seq(1,(prod(cd)),1)),  # ids are numbers between 1 and ns*nl
                               proj4string = crs(r) )

# Save the "raster fishnet"
out_raster   <- raster(sp_grd) %>%
                setValues(sp_grd@data$id)
temprast     <- tempfile(tmpdir = tempdir(), fileext = ".tif")
writeRaster(out_raster, temprast, overwrite = TRUE)

# "supersample" the raster of the cell numbers

ss_factor = 20  # this indicates how much you increase resolution of the "cells" raster
                # the higher this is, the lower the error in computed percentages

temprast_hr  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
super_raster <- gdalwarp(temprast, temprast_hr, tr = res(r)/ss_factor, output_Raster = TRUE, overwrite = TRUE)

# Now rasterize the input polygons with same extent and resolution of super_raster
temprastpoly <- tempfile(tmpdir = tempdir(), fileext = ".tif")
rastpoly = gdal_rasterize(tempshapefile, temprastpoly, tr = raster::res(super_raster),
                           te = extent(super_raster)[c(1,3,2,4)], a = 'id', output_Raster = TRUE)

# Compute Zonal statistics: for each "value" of the  supersampled fishnet raster, compute the number
# of cells which  have a non-zero value  in the supersampled polygons raster (i.e., they belong to one polygon)

cell_nos <- getValues(super_raster)
polyid   <- getValues(rastpoly)
rDT <- data.table(polyid_fc = as.numeric(polyid), cell_nos = as.numeric(cell_nos))
setkey(rDT, cell_nos)

# Use data.table to summarize over cell numbers
count <- rDT[, lapply(.SD, FUN = function(x, na.rm = TRUE) {
                                        100*length(which(x > 0))/(ss_factor^2)
                                          },
                            na.rm = na.rm),
             by = cell_nos]

# Put the results back in the SpatialGridDataFrame and plot
sp_grd@data <- data.frame(count)
sp_grd$polyid_fc[sp_grd$polyid_fc == 0] <- NA
spplot(sp_grd, zcol = 'polyid_fc')

