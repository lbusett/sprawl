library(bitops)
library(foreach)
library(doSNOW)
library(sp)
library(dplyr)
library(anytime)

if (!file.exists(in_lc_modis_file)) {
  in_lc_file     <- raster::raster("/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif")
  out_lc_file    <- "/home/lb/Temp/buttami/MOD13/Italy_mask_ARABLE.tif"
  dir.create(dirname(out_lc_file))
  in_zones_layer <- raster::raster("/home/lb/projects/ermes/datasets/rs_products/MODIS/IT/VI_16Days_250m_v6/EVI/MOD13Q1_EVI_2003_001.dat")
  t1 = Sys.time()
  aggr_lc_raster <- aggregate_rast(in_lc_file,
                                   in_zones_layer,
                                   FUN = mean,
                                   method = "fastdisk",
                                   to_file = TRUE,
                                   out_file = out_lc_file,
                                   verbose = TRUE,
                                   maxchunk = 50E6)
  Sys.time() - t1
}


if (!file.exists(in_lc_ermes_file)) {
  in_lc_file     <- raster::raster("/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif")
  out_lc_file    <- "/home/lb/Temp/buttami/MOD13/Italy_mask_ARABLE_ERMES2x2.tif"
  dir.create(dirname(out_lc_file))
  in_zones_layer <- raster::raster("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_001.tif")
  t1 = Sys.time()
  aggr_lc_raster <- aggregate_rast(in_lc_file,
                                   in_zones_layer,
                                   FUN = mean,
                                   method = "fastdisk",
                                   to_file = TRUE,
                                   out_file = out_lc_file,
                                   verbose = TRUE,
                                   maxchunk = 50E6)
  Sys.time() - t1
}
