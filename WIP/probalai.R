

#   ____________________________________________________________________________
#   get the two LAI hdf images and convert to raster, then mosaic them     ####

in_fold <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Raw_data/20170603/"
in_hdfs <- list.files(in_fold, pattern = "*.h5", full.names = T)
outras <- tempfile(fileext = ".tif")
lai_18 <- gdal_translate(in_hdfs[1], outras, sd_index = 1, output_Raster = T)
extent(lai_18) <- c(0,10,40,50)
proj4string(lai_18) <- "+init=epsg:4326"

outras <- tempfile(fileext = ".tif")
lai_19 <- gdal_translate(in_hdfs[2], outras, sd_index = 1, output_Raster = T)
extent(lai_19) <- c(10,20,40,50)
proj4string(lai_19) <- "+init=epsg:4326"

lai_tot <- mosaic(lai_18, lai_19, fun = mean, na.rm = T)

#   ____________________________________________________________________________
#   get the two quality hdf images and convert to raster, then mosaic them     ####

outras <- tempfile(fileext = ".tif")
qual_18 <- gdal_translate(in_hdfs[1], outras, sd_index = 3, output_Raster = T)
extent(qual_18) <- c(0,10,40,50)
proj4string(qual_18) <- "+init=epsg:4326"

outras <- tempfile(fileext = ".tif")
qual_19 <- gdal_translate(in_hdfs[2], outras, sd_index = 3, output_Raster = T)
extent(qual_19) <- c(10,20,40,50)
proj4string(qual_19) <- "+init=epsg:4326"

qual_tot <- mosaic(qual_18, qual_19, fun = mean, na.rm = T)


extent(lai_18) <- c(0,10,40,50)
proj4string(lai)


f <- h5file(inras)
h5ls(inras)
mydata <- h5read(inras, "LAI")
myerr <- h5read(inras, "LAI-ERR")
myqual <- h5read(inras, "LAI-QFLAG")
h5readAttributes(inras, "LAI")




pro <- raster(t(mydata),
              xmn = 0,
              xmx = 10,
              ymn = 40,
              ymx = 50,
              crs = "+init=epsg:4326"
              )
pro2 <- raster(t(myqual),
              xmn = 0,
              xmx = 10,
              ymn = 40,
              ymx = 50,
              crs = "+init=epsg:4326"
)

bits <- 0
in_values <- t(myqual)
bitfield_vals <- bitShiftR(in_values, bits)
bitfield_vals <- bitAnd(bitfield_vals, 2^(1) - 1)

pro3 = raster(pro2)
values(pro3) <- (bitfield_vals)
plot(pro3)


