library(bitops)
#   ____________________________________________________________________________
#   get the two LAI hdf images and convert to raster, then mosaic them     ####

in_folds            <- list.files("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Raw_data",
                                  pattern = "[0-9]", full.names = T)

for (fold in in_folds) {
  print(fold)

  in_hdfs             <- list.files(fold, pattern = "*.h5$", full.names = T)
  outras              <- tempfile(fileext = ".tif")
  lai_18              <- gdalUtils::gdal_translate(in_hdfs[1], outras, sd_index = 1, output_Raster = T)
  extent(lai_18)      <- c(0,10,40,50)
  proj4string(lai_18) <- "+init=epsg:4326"
  outras              <- tempfile(fileext = ".tif")
  lai_19              <- gdalUtils::gdal_translate(in_hdfs[2], outras, sd_index = 1, output_Raster = T)
  extent(lai_19)      <- c(10,20,40,50)
  proj4string(lai_19) <- "+init=epsg:4326"
  lai_tot             <- raster::mosaic(lai_18, lai_19, fun = mean, na.rm = T)

  #   ____________________________________________________________________________
  #   get the two quality hdf images and convert to raster, then mosaic them     ####

  outras               <- tempfile(fileext = ".tif")
  qual_18              <- gdalUtils::gdal_translate(in_hdfs[1], outras, sd_index = 3, output_Raster = T)
  extent(qual_18)      <- c(0,10,40,50)
  proj4string(qual_18) <- "+init=epsg:4326"
  outras               <- tempfile(fileext = ".tif")
  qual_19              <- gdalUtils::gdal_translate(in_hdfs[2], outras, sd_index = 3, output_Raster = T)
  extent(qual_19)      <- c(10,20,40,50)
  proj4string(qual_19) <- "+init=epsg:4326"
  qual_tot             <- raster::mosaic(qual_18, qual_19, fun = mean, na.rm = T)

  #   ____________________________________________________________________________
  #   get the two rmse hdf images and convert to raster, then mosaic them     ####

  outras               <- tempfile(fileext = ".tif")
  rmse_18              <- gdalUtils::gdal_translate(in_hdfs[1], outras, sd_index = 2, output_Raster = T)
  extent(rmse_18)      <- c(0,10,40,50)
  proj4string(rmse_18) <- "+init=epsg:4326"
  outras               <- tempfile(fileext = ".tif")
  rmse_19              <- gdalUtils::gdal_translate(in_hdfs[2], outras, sd_index = 2, output_Raster = T)
  extent(rmse_19)      <- c(10,20,40,50)
  proj4string(rmse_19) <- "+init=epsg:4326"
  rmse_tot             <- raster::mosaic(rmse_18, rmse_19, fun = mean, na.rm = T)


  #   ____________________________________________________________________________
  #   Resize both based on ERMES extent                                       ####
  in_ERMES_LAI_ex <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_009.tif" %>%
    raster::raster()
  extent_ERMES  <- reproj_extent(extent(in_ERMES_LAI_ex),
                                 in_proj = sp::proj4string(in_ERMES_LAI_ex),
                                 out_proj = sp::proj4string(lai_tot))

  lai_cropped  <- raster::crop(lai_tot, extent_ERMES + 0.12)
  qual_cropped <- raster::crop(qual_tot, extent_ERMES + 0.12)
  rmse_cropped <- raster::crop(rmse_tot, extent_ERMES + 0.12)

  #   ____________________________________________________________________________
  #   build a mask based on quality values                                    ####

  qual_values  <- getValues(qual_cropped)
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
    0,   211, 1,
    211, 255, NA)
  laival_mask <- reclass_rast(lai_cropped, rcl_mat)

  rcl_mat <- tibble::tribble(
    ~start, ~end, ~new,
    0,  75, 1,
    75, 260, NA)
  rmse_mask   <- reclass_rast(rmse_cropped, rcl_mat)

  #   ____________________________________________________________________________
  #   build final mask and apply it to lai values                             ####

  mask_rast <- mask_rast * laival_mask * rmse_mask
  lai_rast <- (lai_cropped * mask_rast) / 30

  #   ____________________________________________________________________________
  #   aggregate arable land raster to PROBA grid                              ####
  in_lc_proba_file <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/Ancillary/ERMES_fc_ARABLE_proba.tif"
  if (!file.exists(in_lc_proba_file)) {
    in_lc_file     <- raster::raster("/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif")
    in_zones_layer <- lai_rast
    aggr_lc_raster <- aggregate_rast(in_lc_file,
                                     in_zones_layer,
                                     FUN = mean,
                                     method = "fastdisk",
                                     to_file = TRUE,
                                     out_file = in_lc_proba_file,
                                     verbose = FALSE)
  }

  rcl_mat <- tibble::tribble(
    ~start, ~end, ~new,
    0,   0.7, NA,
    0.7, 100, 1)
  fc_proba_mask <- reclass_rast(raster(in_lc_proba_file), rcl_mat)

  #   ____________________________________________________________________________
  #   Mask LAI values below 0.6 arable land fractional cover                  ####
  lai_final_cropped <- lai_rast * fc_proba_mask

  #   ____________________________________________________________________________
  #   finally, aggregate on the 2x2 ERMES grid                                ####
  grid_example <- raster::raster("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_001.tif")
  out_fold <- file.path("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT", "2k_grid")
  dir.create(out_fold, recursive = T, showWarnings = F)
  out_file <- file.path(out_fold, paste0("LAI_Proba_", basename(fold), ".tif"))

  aggr_lai <- aggregate_rast(lai_final_cropped,
                             grid_example,
                             FUN      = mean,
                             method   = "fastdisk",
                             to_file  = TRUE,
                             out_file = out_file,
                             verbose  = FALSE)
}

out_list <- list.files("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/VGT/2k_grid",
                       full.names = T)
pippo = stack(out_list)
