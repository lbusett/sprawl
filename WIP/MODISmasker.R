#' MODISmasker
#'
#'
#' @importFrom raster raster extent intersect
#' @importFrom sp proj4string
#' @importFrom gdalUtils gdalwarp
#' @return
#' @export
#'
#' @examples
#'

MODISmasker <- function(){

  in_maskfile <- "/home/lb/projects/ermes/datasets/rs_products/RICE_map/Gambia/Delivery/Classification2016_Geographic"

  mod_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

  # Open the raster mask - if necessary do something to convert it to 0-1
  if (class(in_maskfile) == "character") {
    in_mask <- raster(in_maskfile)
  }

  # Intervals are CLOSED on THE LEFT and OPEN ON THE RIGHT !
  # Therefore, start = 1, end = 5 means from 1 to 4.999999, etcetera !
  rcl_mat <- list(
    list(start = 0, end  = 0, new = NA),
    list(start = 1, end  = 5, new = 0),
    list(start = 5, end  = 6, new = 1),
    list(start = 6, end  = 9, new = 0),
    list(start = 9, end  = 11, new = 1),
    list(start = 11, end = 100, new = NA)
  )


  class(rcl_mat) <- "reclass_matrix"

  reclass_file = "/home/lb/Temp/buttami/Classification2016_Geographic_reclass.tif"
  outmask = rast_reclass(in_mask, rcl_mat, out_rast = reclass_file, r_out = TRUE, ovr = TRUE)

  # in_mask     <- raster(mask_reclass_file)
  in_maskfile <- reclass_file
  #

  # Get mask projection information and extent
  mask_proj <- proj4string(outmask)
  ext_mask  <- extent(outmask)

  # if necessary, reproject the input mask
  mask_reprojfile <- tempfile(tmpdir= tempdir(),fileext = ".tif")
  mask_reprojfile <- "/home/lb/Temp/buttami/mask_Gambia_Rice_Other_mang.dat"

  gdalwarp(in_maskfile, mask_reprojfile, s_srs = mask_proj,
           t_srs = mod_proj, overwrite = TRUE, ot = 'Byte', of = "ENVI")

  # Convert mask extent to MODIS projection
  mask_repr     <- raster(mask_reprojfile)
  ext_mask_sinu <- extent(mask_repr)


  # find the MODIs tiles which intersect the mask
  int_tiles <- intersect(modis_grid, ext_mask_sinu)
  minh <- min(int_tiles$H)
  maxh <- max(int_tiles$H)
  minv <- min(int_tiles$V)
  maxv <- max(int_tiles$V)

  submod_grid <- subset(modis_grid, (H >= minh & H <= maxh & V >= minv & V <= maxv))

  # Create a MODIS polygon grid spanning the identified tiles and crop it on
  # extent of the mask

  # Create a polygon grid encompassing the extent of the tiles
  in_ext  <- extent(submod_grid)
  zones_raster <- "/home/lb/Temp/buttami/fishrastw.tif"

  create_fishnet(in_ext, crop_ext =  ext_mask_sinu,  out_shape = FALSE, cellsize = 231.656358,
                            in_proj = mod_proj, out_rastfile = zones_raster, overw = TRUE
                            )
  # make so that the mask and the zones tiff have the same extent and resolution

  zones_raster_hr <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  gdal_translate(zones_raster, zones_raster_hr, tr = raster::res(mask_repr),
           te = extent(mask_repr)[c(1,3,2,4)], overwrite = T, tap = T)
  mask_alignedfile <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  align_rasters(mask_reprojfile, zones_raster_hr, mask_alignedfile)

  # Compute zonal statistics on MODIS pixels using the mask as input and a user function to extract
  # values from the mask

  zonestats <- fastzonal(mask_repr, sp_object = zones_raster_hr)


  # End processing


####################################################################################

  in_maskfile <- "/home/lb/projects/ermes/datasets/rs_products/RICE_map/Gambia/Delivery/Classification2016_Geographic"
  mod_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

  # Open the raster mask - if necessary do something to convert it to 0-1
  in_mask <- raster(in_maskfile)


  # Intervals are CLOSED on THE LEFT and OPEN ON THE RIGHT !
  # Therefore, start = 1, end = 5 means from 1 to 4.999999, etcetera !
  rcl_mat <- list(
    list(start = 0, end  = 0, new = NA),
    list(start = 1, end  = 5, new = 0),
    list(start = 5, end  = 6, new = 1),
    list(start = 6, end  = 9, new = 0),
    list(start = 9, end  = 11, new = 1),
    list(start = 11, end = 100, new = NA)
  )


  class(rcl_mat) <- "reclass_matrix"

  reclass_file = "/home/lb/Temp/buttami/Classification2016_Geographic_reclass.tif"
  outmask = rast_reclass(in_mask, rcl_mat, out_rast = reclass_file, r_out = TRUE, ovr = TRUE)

  # in_mask     <- raster(mask_reclass_file)
  in_maskfile <- reclass_file
  mask_proj <- proj4string(outmask)
  ext_mask  <- extent(outmask)

  # if necessary, reproject the input mask
  mask_reprojfile <- tempfile(tmpdir= tempdir(),fileext = ".tif")
  mask_reprojfile <- "/home/lb/Temp/buttami/mask_Gambia_Rice_Other_mang.dat"

  gdalwarp(in_maskfile, mask_reprojfile, s_srs = mask_proj,
           t_srs = mod_proj, overwrite = TRUE, ot = 'Byte', of = "ENVI")

  mask_repr     <- raster(mask_reprojfile)
  ext_mask_sinu <- extent(mask_repr)
  zones_rast <- "/home/lb/Temp/buttami/fishrastw.tif"

  # find the MODIs tiles which intersect the mask
  int_tiles <- intersect(modis_grid, ext_mask_sinu)
  minh <- min(int_tiles$H)
  maxh <- max(int_tiles$H)
  minv <- min(int_tiles$V)
  maxv <- max(int_tiles$V)

  submod_grid <- subset(modis_grid, (H >= minh & H <= maxh & V >= minv & V <= maxv))

  # Create a MODIS polygon grid spanning the identified tiles and crop it on
  # extent of the mask

  # Create a polygon grid encompassing the extent of the tiles
  in_ext  <- extent(submod_grid)


  create_fishnet(in_ext, crop_ext =  ext_mask_sinu,  out_shape = FALSE, cellsize = 231.656358,
                 in_proj = mod_proj, out_rastfile = zones_rast, overw = TRUE
  )

  zones_raster_hr <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  gdal_translate(zones_rast, zones_raster_hr, tr = raster::res(mask_repr),
                 te = extent(mask_repr)[c(1,3,2,4)], overwrite = T, tap = T)
  mask_alignedfile <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  align_rasters(mask_reprojfile, zones_raster_hr, mask_alignedfile)

  # Compute zonal statistics on MODIS pixels using the mask as input and a user function to extract
  # values from the mask

  zonestats <- fastzonal(raster(mask_alignedfile), sp_object = zones_raster_hr, out_format = 'dframe')

  outrast   <-  raster(zones_rast)
  outrast[] <- (as.numeric(zonestats[2:length(names(zonestats))]))

  rcl_mat <- list(
    list(start = 0, end  = 0.74999, new = 0),
    list(start = 0.75, end  = 1.1, new = 1)
  )
  outfile <- "/home/lb/Temp/buttami/Gmabia_lcMask_075.envi"
  outmask = rast_reclass(outrast, rcl_mat, out_rast = outfile, r_out = TRUE, ovr = TRUE)

}
