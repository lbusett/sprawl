library(bitops)
library(foreach)
library(doSNOW)
library(sp)
library(dplyr)
library(anytime)
library(sprawl)
#   ____________________________________________________________________________
#   get the two LAI hdf images and convert to raster, then mosaic them     ####

in_folds <- list.files("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Raw_data",
                       pattern = "[0-9]", full.names = T)
in_folds <- in_folds[2:length(in_folds)]
in_folds <- in_folds[21:25]
in_ERMES_LAI_ex <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_009.tif" %>%
  raster::raster()

in_lc_proba_file <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Ancillary/ERMES_fc_ARABLE_proba.tif"

grid_example <- raster::raster("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_001.tif")

out_fold <- file.path("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT", "2k_grid")
dir.create(out_fold, recursive = T, showWarnings = F)

#   ____________________________________________________________________________
#   aggregate arable land raster to PROBA grid to create the arable land    ####
#   mask

if (!file.exists(in_lc_proba_file)) {
  in_lc_file     <- raster::raster("/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif")
  in_zones_layer <- lai_cropped
  t1 = Sys.time()
  aggr_lc_raster <- aggregate_rast(in_lc_file,
                                   in_zones_layer,
                                   FUN = mean,
                                   method = "fastdisk",
                                   to_file = TRUE,
                                   out_file = in_lc_proba_file,
                                   verbose = TRUE,
                                   maxchunk = 5E6)
  Sys.time() - t1
}

rcl_mat <- tibble::tribble(
  ~start, ~end, ~new,
  0,   0.7, NA,
  0.7, 100, 1)
fc_proba_mask <- sprawl::reclass_rast(raster::raster(in_lc_proba_file), rcl_mat)

cl <-  parallel::makeCluster(4)
doSNOW::registerDoSNOW(cl)
pb       <- utils::txtProgressBar(max = length(in_folds), style = 3)
progress <- function(n) utils::setTxtProgressBar(pb, n)
opts     <- list(progress = progress)

results <- foreach::foreach(folder = seq_along(in_folds),
                            .packages = c("gdalUtils", "raster", "dplyr", "tibble",
                                          "sprawl", "magrittr", "bitops", "anytime"),
                            .options.snow = opts,
                            .verbose = FALSE) %dopar% {

                              message("Working on band: ", folder, " of: ", length(in_folds))

                              fold     <- in_folds[folder]
                              in_hdfs  <- list.files(fold, pattern = "*.h5$", full.names = T)
                              doy_acq  <- datetodoy(anydate(basename(fold), tz = "UTC"))
                              out_file <- file.path(out_fold, paste0("IT_LAI_VGT_2017_", sprintf("%03d", doy_acq), ".tif"))

                              #   ____________________________________________________________________________
                              #   get the two lai hdf images and convert to raster, then mosaic them     ####

                              lai_18 <- gdalUtils::gdal_translate(in_hdfs[1], tempfile(fileext = ".tif"), sd_index = 1, output_Raster = T)
                              lai_19 <- gdalUtils::gdal_translate(in_hdfs[2], tempfile(fileext = ".tif"), sd_index = 1, output_Raster = T)

                              #   ____________________________________________________________________________
                              #   get the two quality hdf images and convert to raster, then mosaic them     ####

                              qual_18 <- gdalUtils::gdal_translate(in_hdfs[1], tempfile(fileext = ".tif"), sd_index = 3, output_Raster = T)
                              qual_19 <- gdalUtils::gdal_translate(in_hdfs[2], tempfile(fileext = ".tif"), sd_index = 3, output_Raster = T)

                              #   ____________________________________________________________________________
                              #   get the two rmse hdf images and convert to raster, then mosaic them     ####

                              rmse_18 <- gdalUtils::gdal_translate(in_hdfs[1], tempfile(fileext = ".tif"), sd_index = 2, output_Raster = T)
                              rmse_19 <- gdalUtils::gdal_translate(in_hdfs[2], tempfile(fileext = ".tif"), sd_index = 2, output_Raster = T)

                              #   ____________________________________________________________________________
                              #   set correct extents and projections                                     ####

                              raster::extent(lai_18)  <-
                                raster::extent(qual_18) <-
                                raster::extent(rmse_18)  <- c(0,10,40,50)

                              raster::extent(lai_19)  <-
                                raster::extent(qual_19) <-
                                raster::extent(rmse_19)  <- c(10,20,40,50)

                              lai_tot   <- raster::mosaic(lai_18, lai_19, fun = mean, na.rm = T)
                              qual_tot  <- raster::mosaic(qual_18, qual_19, fun = mean, na.rm = T)
                              rmse_tot  <- raster::mosaic(rmse_18, rmse_19, fun = mean, na.rm = T)

                              lai_18 <- lai_19 <- qual_18 <- qual_19 <- rmse_18 <- rmse_19 <- NULL

                              sp::proj4string(lai_tot) <- sp::proj4string(qual_tot) <-
                                sp::proj4string(rmse_tot) <- "+init=epsg:4326"
                              #   ____________________________________________________________________________
                              #   Resize both based on ERMES extent                                       ####

                              extent_ERMES  <- sprawl::reproj_extent(raster::extent(in_ERMES_LAI_ex),
                                                                     in_proj = sp::proj4string(in_ERMES_LAI_ex),
                                                                     out_proj = sp::proj4string(lai_tot))

                              lai_cropped  <- raster::crop(lai_tot, extent_ERMES@extent + 0.12)
                              qual_cropped <- raster::crop(qual_tot, extent_ERMES@extent + 0.12)
                              rmse_cropped <- raster::crop(rmse_tot, extent_ERMES@extent + 0.12)

                              #   ____________________________________________________________________________
                              #   build a mask based on quality values                                    ####

                              qual_values  <- raster::getValues(qual_cropped)
                              water_vals   <- as.numeric(!bitShiftR(qual_values, 0)  %>% bitAnd(1))
                              filled_vals  <- as.numeric(!bitShiftR(qual_values, 3)  %>% bitAnd(1))
                              laiqual_vals <- as.numeric(!bitShiftR(qual_values, 7)  %>% bitAnd(1))
                              ebf_vals     <- as.numeric(!bitShiftR(qual_values, 11) %>% bitAnd(1))
                              bare_vals    <- as.numeric(!bitShiftR(qual_values, 12) %>% bitAnd(1))
                              gap_vals     <- as.numeric(!bitShiftR(qual_values, 14) %>% bitAnd(1))

                              mask         <- water_vals * laiqual_vals * ebf_vals * bare_vals * filled_vals * gap_vals
                              mask_rast    <- raster::raster(qual_cropped) %>%
                                raster::setValues(mask)

                              #   ____________________________________________________________________________
                              #   build masks based on LAI and rmse values                                ####

                              rcl_mat <- tibble::tribble(
                                ~start, ~end, ~new,
                                211, 255, NA)
                              lai_cropped <- reclass_rast(lai_cropped, rcl_mat)

                              # rcl_mat <- tibble::tribble(
                              #   ~start, ~end, ~new,
                              #   0,  210, 1,
                              #   210, 260, NA)
                              # rmse_cropped   <- reclass_rast(rmse_cropped, rcl_mat)

                              #   ____________________________________________________________________________
                              #   build final mask and apply it to lai values                             ####

                              # mask_rast <- mask_rast * rmse_cropped * fc_proba_mask
                              mask_rast   <- mask_rast * fc_proba_mask
                              lai_cropped <- (lai_cropped * mask_rast) / 30

                              #   ____________________________________________________________________________
                              #   finally, aggregate on the 2x2 ERMES grid                                ####

                              aggr_lai <- aggregate_rast(lai_cropped,
                                                         grid_example,
                                                         FUN      = mean,
                                                         method   = "fastdisk",
                                                         to_file  = TRUE,
                                                         out_file = out_file,
                                                         verbose  = FALSE ,
                                                         nodata_out = -1)
                              processed <- data.frame("proc_fold" = folder)
                              return(processed)
                            }


stopCluster(cl)
out_list <- list.files("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/2k_grid",
                       full.names = T)
pippo = raster::stack(out_list)
