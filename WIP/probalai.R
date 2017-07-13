library(rhdf5)
inras <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Raw_data/20170603/g2_BIOPAR_LAI_201706030000_H18V4_PROBAV_V1.4.h5"
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


