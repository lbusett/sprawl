#' @title return the proj4string of a spatial object or file
#' @description helper function used to extract the proj4string of "R" spatial objects
#'   or of raster or vector files
#' @param in_obj `character` corresponding to the name of a R object, or a filename
#' (full path)
#' @return `character` proj4string of the object or file
#' @details DETAILS
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom sp proj4string
#' @importFrom sf st_crs
#' @name get_proj4string
#' @rdname get_proj4string
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @examples
#' \dontrun{
#' library(raster)
#'
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif", package = "sprawl.data")
#' get_proj4string(in_rast)
#'
#' in_rast <- raster::raster(in_rast)
#' get_proj4string(in_rast)
#'
#' in_vect <- system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data")
#' get_proj4string(in_vect)
#'
#' in_vect <- read_vect(in_vect)
#' get_proj4string(in_vect)
#' }
#'
get_proj4string <- function(in_obj) {
  UseMethod("get_proj4string")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_proj4string
#' @method get_proj4string default
#' @export
get_proj4string.default  <- function(in_obj) {
  call = match.call()
  stop("get_proj4string --> ", call[[2]],
       " is not a valid vector or raster `R` object or filename. Aborting !")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_proj4string
#' @method get_proj4string character
#' @importFrom rgdal checkCRSArgs
#' @importFrom magrittr "%>%"
#' @export
get_proj4string.character <- function(in_obj) {

  call = match.call()
  #   __________________________________________________________________________
  #   return immediately if a valid projstring was passed.                 ####
  #

  if (!file.exists(in_obj)) {
    projargs <- check_proj4string(in_obj, abort = TRUE)
    if (projargs != "invalid") {
      return(projargs)
    }
    # If all fails, abort.
    stop("get_proj4string --> ", call[[2]], " is not a valid proj4 string ",
         "or spatial file name. Aborting!")
  }
  #   __________________________________________________________________________
  #   Otherwise, check if the character string corresponds to a valid spatial
  #   object and retrieve its proj4

  obj_type <- get_spatype(in_obj, abort = TRUE)
  if (obj_type %in% c("rastfile", "vectfile")) {

    projargs  <- as.character(
      gdalUtils::gdalsrsinfo(in_obj, as.CRS = TRUE)) %>%
      check_proj4string(abort = TRUE)
    return(projargs)
  } else {

    stop("get_proj4string --> ", call[[2]], "is not a valid raster or vector
           file. Aborting!")

  }
}

get_proj4string.numeric <- function(in_obj) {

  call = match.call()
  #   __________________________________________________________________________
  #   Try to interpret a number as a valid EPSG code or UTM zone            ####
  #
  projargs <- check_proj4string(in_obj, abort = TRUE)
  return(projargs)
}

#   ____________________________________________________________________________
#   Method for "rastobj" - use sp::proj4string                              ####

#' @rdname get_proj4string
#' @method get_proj4string Raster
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.Raster <- function(in_obj) {

  projargs  <- sp::proj4string(in_obj) %>%
    check_proj4string(abort = TRUE)
  return(projargs)
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_crs                                 ####
#

#' @rdname get_proj4string
#' @method get_proj4string sf
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.sf <- function(in_obj) {

  projargs <- sf::st_crs(in_obj)$proj4string %>%
    check_proj4string(abort = TRUE)
  return(projargs)

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_proj4string
#' @method get_proj4string sfc
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.sfc <- function(in_obj) {

  projargs <- sf::st_crs(in_obj)$proj4string %>%
    check_proj4string(abort = TRUE)
  return(projargs)

}

#   ____________________________________________________________________________
#   Method for "Spatial" object - use sp::proj4string(object)               ####
#

#' @rdname get_proj4string
#' @method get_proj4string Spatial
#' @export
get_proj4string.Spatial <- function(in_obj) {

  projargs  <- sp::proj4string(in_obj) %>%
    check_proj4string(abort = TRUE)
  return(projargs)

}

#   ____________________________________________________________________________
#   Method for "sprawlext" object - read the CRS                            ####
#

#' @rdname get_proj4string
#' @method get_proj4string Spatial
#' @export
get_proj4string.sprawlext <- function(in_obj) {

  return(in_obj@proj4string)

}
