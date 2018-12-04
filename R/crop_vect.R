#' @title crop a vector on the extent of a different spatial file
#' @description Function allowing to crop an input vector object or shapefile on the extent of a
#' different spatial object. in_vect must be an *sp, *sf object or vector file, while ext_obj should
#' be a valid spatial object (sp, sf, vector file, raster or raster file name)
#' @param in_vect Object of class `sfc_POLYGON` or `sf_POLYGON`, or coercible to
#'   it using (sprawl::cast_vect)
#' @param ext_obj Object of class `sprawl_ext`, or from which an extent can be
#'   retrieved using `spralw::get_extent` (i.e., any `R` spatial object, or
#'   valid filenames of raster or vector files)
#' @param out_file PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'
#'  library(raster)
#'  library(sf)
#'  in_polys <- read_vect(system.file("extdata/shapes", "lc_polys.shp",
#'                                          package = "sprawl.data"))
#'  in_rast <- raster::raster(system.file("extdata/MODIS_test",
#'                                   "EVIts_test.tif", package = "sprawl.data"))
#'  in_polys_cropped <- crop_vect(in_polys, in_rast)
#'
#'  # Retrieve a polygon representing in_rast extent and reproject it
#'  rast_extent <- as(get_extent(in_rast), "sfc_POLYGON") %>%
#'     sf::st_transform(get_proj4string(in_polys))
#'  par(mfrow=c(1,2))
#'  plot(in_polys[1])
#'  plot(rast_extent, add = TRUE)
#'  plot(in_polys_cropped[1])
#'  plot(rast_extent, add = TRUE)
#'   par(mfrow=c(1,2))
#'
#' @seealso
#'  \code{\link[sp]{proj4string}}
#' @rdname crop_vect
#' @export
#' @importFrom raster extent raster crop
#' @importFrom sf st_as_sf st_bbox st_crs st_as_sfc st_set_agr st_transform st_intersection
#' @importFrom sp proj4string
#'
crop_vect <- function(in_vect,
                      ext_obj,
                      out_file = NULL,
                      verbose  = TRUE) {

  call = match.call()
  if (verbose) message("crop_vect --> cropping ", call[[2]],
                       " on extent of ", call[[3]])
  #TODO Modify to use get_extent

  #   __________________________________________________________________________
  #   Check arguments                                                       ####
  vectype <- get_vectype(in_vect)
  objtype <- get_spatype(ext_obj)

  if (vectype == "sprawlext") {
    in_vect <- as(in_vect, "sfc_POLYGON")
  } else {
    in_vect <- cast_vect(in_vect, "sfobject")
  }

  obj_bbox    <- get_extent(ext_obj)
  invect_proj <- get_proj4string(in_vect)
  inobj_proj  <- get_proj4string(ext_obj)
  obj_boundaries <- .sprawlext_to_poly(obj_bbox)
  if (invect_proj != inobj_proj) {
    obj_boundaries <- sf::st_transform(obj_boundaries, invect_proj)
  }

  if (inherits(in_vect, "sf")) {
    sf::st_agr(in_vect) = "constant"
  }
  cropped_vect <- sf::st_intersection(in_vect, obj_boundaries)

  if (!is.null(out_file)) {
    write_shape(cropped_vect, out_file)
  }
  return(cropped_vect)
}
