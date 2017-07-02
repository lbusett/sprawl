file <- "http://ncss.hycom.org/thredds/ncss/GLBa0.08/latest/temp?var=temperature&north=25&west=74.1199&east=434.1199&south=-15&horizStride=1&time_start=2016-09-27T00%3A00%3A00Z&time_end=2016-09-27T23%3A59%3A00Z&timeStride=1&vertCoord=&accept=netcdf4"
savefile = tempfile(, fileext = "nc4")
download.file(file, savefile)
library(ncdf4)
ncdata <- nc_open("/home/lb/Temp/buttami/hycom.nc4")
lon <- ncvar_get(ncdata, "Longitude")
lat <- ncvar_get(ncdata, "Latitude")
temp <-ncvar_get(ncdata,"temperature")

temp <- temp [,,1:10] # subset to speed up things...
depths <- 1:10  # let's define some dummy depths - you want to put actual values, here !

finddepth = function(pixtemp, ...) {
  if (max(pixtemp, na.rm = TRUE) < predtemp$temp) {
    NA    # set to NA if no values >= 25
  } else {
    depth <- tryCatch({
      depth <- approx(pixtemp, depths,predtemp$temp)$y # interpolate using linear (faster)
      # browser()
      # interp  <- loess(depths~pixtemp)  # interpolate using loess  (slower - deals with non-linearity)
      # depth  <- predict(interp, predtemp$temp) # find depth @ temperature
      return(depth)   # send back computed depth
    }, error = function(e) NA)

  }
}
predtemp  <- data.frame(temp = 25)   # set desired isotherm
iso_depth <- apply(temp, c(1, 2), FUN = finddepth)




setwd("~/Desktop/CMORPH/Levant-Clip/200001")

dir.output <- '~/Desktop/CMORPH/Levant-Clip/200001' ### change as needed to give output location
path <- list.files("~/Desktop/CMORPH/MonthlyCMORPH/200001",pattern="*.bz2", full.names=T, recursive=T)
raster_list = list()
for (i in 1:length(path)) {
  files = bzfile(path[i], "rb")
  data <- readBin(files,what="double",endian = "little", n = 4948*1649, size=4) #Mode of the vector to be read
  data [500] <- 1
  data[data == -999] <- NA #covert missing data from -999(CMORPH notation) to NAs
  y<-matrix((data=data), ncol=1649, nrow=4948)
  r <- raster(y)
  if (i == 1) {
    e <- extent(-180, 180, -90, 83.6236) ### choose the extent based on the netcdf file info
  }
  tr <- t(r) #transpose
  re <- setExtent(tr,extent(e)) ### set the extent to the raster
  ry <- flip(re, direction = 'y')
  projection(ry) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
  C_Lev <- crop(ry, Levant) ### Clip to Levant
  M_C_Lev<-mask(C_Lev, Levant)
  raster_list[[i]] = M_C_Lev
}
#


rasstk <- stack(raster_list, quick = TRUE) # OR rasstk <- brick(raster_list, quick = TRUE)
avg200001<-mean(rasstk)
writeRaster(avg200001, paste(dir.output, basename(path[i]), sep = ''), format = 'GTiff', overwrite = T) ###the basename allows the file to be named the same as the original


#For testing purposes here I substituted your cycle on file names with a simple 1 to 720 cycle and your
# file reading with the creation of arrays of the same length !

ptm <- proc.time()
totdata = array(dim = 4948*1649)  # Define Dummy array
for (i in 1:720) {
  message("Working on file: ", i)
  data <- array(rep(c(1,2,3,4),4948*1649/4), dim = 4948*1649) # Create a "fake" 4948*1649 array  each time to simulate data reading
  data[1:1000] <- -999   # Set some values to NA
  data[data == -999] <- NA #covert missing data from -999

  totdata <- rowSums(cbind(totdata, data), na.rm = T)   # Let's sum the current array with the cumulative sum so far
}

# Now reshape to matrix and convertt to raster, etc.
y  <- matrix(totdata, ncol=1649, nrow=4948)
r  <- raster(y)
e  <- extent(-180, 180, -90, 83.6236) ### choose the extent based on the netcdf file info
tr <- t(r) #transpose
re <- setExtent(tr,e) ### set the extent to the raster
ry <- flip(re, direction = 'y')
projection(ry) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
proc.time() - ptm

mapview(ry, legend = T)





# Now do the "spatial" processing

y<-matrix(avg_data, ncol=1649, nrow=4948)
r <- raster(y)
e <- extent(-180, 180, -90, 83.6236) ### choose the extent based on the netcdf file info
tr <- t(r) #transpose
re <- setExtent(tr,extent(e)) ### set the extent to the raster
ry <- flip(re, direction = 'y')
projection(ry) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
C_Lev <- crop(avg_data, Levant) ### Clip to Levant
M_C_Lev<-mask(C_Lev, Levant)
writeRaster(M_C_Lev, paste(dir.output, basename(path[i]), sep = ''), format = 'GTiff', overwrite = T) ###the basename allows the file to be named the same as the original
})


