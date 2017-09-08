#' @title crop a vector on the extent of a different spatial file
#' @description Function allowing to crop an input vector object or shapefile on the extent of a
#' different spatial object. in_vect must be an *sp, *sf object or vector file, while in_obj should
#' be a valid spatial object (sp, sf, vector file, raster or raster file name)
#' @param in_vect PARAM_DESCRIPTION
#' @param in_obj PARAM_DESCRIPTION
#' @param to_file PARAM_DESCRIPTION, Default: FALSE
#' @param as_sp PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
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
#'  plot(rast_extent, add = T)
#'  plot(in_polys_cropped[1])
#'  plot(rast_extent, add = T)
#'  }
#' @seealso
#'  \code{\link[sp]{proj4string}}
#' @rdname crop_vect
#' @export
#' @importFrom raster extent raster crop
#' @importFrom sf st_as_sf st_bbox st_crs st_as_sfc st_set_agr st_transform st_intersection
#' @importFrom sp proj4string
#'
crop_vect <- function(in_vect,
                      in_obj,
                      to_file = FALSE,
                      as_sp   = FALSE) {

  #TODO Modify to use get_extent

  #   __________________________________________________________________________
  #   Check arguments                                                       ####
  vectype <- get_vectype(in_vect)

  objtype <- try(get_rastype(in_obj), silent = T)
  if (class(objtype) == "try-error") {
    objtype <- try(get_vectype(in_obj), silent = T)
  }
  if (class(objtype) == "try-error") {
    stop()
  }
  if (!vectype %in% c("sfobject", "spobject", "vectfile")) {
    stop("crop_vect --> in_vect is not a valid vector object (sp, sf or ",
         "vector file")
  }
  if (objtype == "none") {
    stop("crop_vect --> in_obj is not a valid spatial object (sp, sf, vector ",
         "file, raster or raster file name) or sprawlext object")
  }
  if (vectype == "sprawlext") {
    in_vect <- as(in_vect, "sfc_POLYGON")
  } else {
    in_vect <- cast_vect(in_vect, "sfobject")
  }
  obj_bbox    <- get_extent(in_obj)
  invect_proj <- get_proj4string(in_vect)
  inobj_proj  <- get_proj4string(in_obj)
  obj_boundaries <- as(obj_bbox, "sfc_POLYGON")
  if (invect_proj != inobj_proj) {
    obj_boundaries <- sf::st_transform(obj_boundaries, invect_proj)
  }

  if (inherits(in_vect, "sf")) {
    sf::st_agr(in_vect) = "constant"
  }
  cropped_vect <- sf::st_intersection(in_vect, obj_boundaries)
  return(cropped_vect)
}
