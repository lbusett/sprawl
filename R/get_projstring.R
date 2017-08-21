#' @title return the proj4string of a spatial object or file
#' @description helper function used to extract the proj4string of "R" spatial objects
#'   or of raster or vector files
#' @param object `character` corresponding to the name of a R object, or a filename
#' (full path)
#' @param abort `logical` if TRUE, the function aborts in case no proj4string or
#'   invalid proj4string is found, Default: FALSE
#' @return `character` proj4string of the object or file
#' @details DETAILS
#' @examples
#' \dontrun{
#'  library(raster)
#'
#'  in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
#'  get_proj4string(in_rast)
#'
#'  in_rast <- raster::raster(in_rast)
#'  get_proj4string(in_rast)
#'
#'  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#'  get_proj4string(in_vect)
#'
#'  in_vect <- read_vect(in_vect)
#'  get_proj4string(in_vect)
#'
#'  }
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom sp proj4string
#' @importFrom sf st_crs
#' @rdname get_proj4string
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
get_proj4string <- function(object,
                            abort = FALSE) {
  UseMethod("get_proj4string")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_proj4string
#' @method get_proj4string default
#' @export
get_proj4string.default  <- function(object, abort = FALSE) {
  if (abort == TRUE) {
    stop("get_proj4string --> `object` is not a valid vector or raster `R` object or
         filename. Aborting!")
  } else {
    warning("get_proj4string --> `object` is not a valid vector or raster `R` object or
            filename. Aborting!")
  }
  }

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_proj4string
#' @method get_proj4string character
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.character <- function(object,
                                      abort = FALSE) {

  obj_type <- get_spatype(object)

  if (obj_type %in% c("rastfile", "vectfile")) {

    proj4string  <- as.character(gdalUtils::gdalsrsinfo(object, as.CRS = TRUE))
    proj4string <- check_proj4string(proj4string, abort = abort)
    return(proj4string)
  } else {
    if (abort == TRUE) {
      stop("get_proj4string --> `object` is not a valid raster or vector
           file, Aborting!")
    } else {
      warning("get_proj4string --> `object` is not a valid raster or vector
              file!")
      return("none")
    }
  }
  }


#   ____________________________________________________________________________
#   Method for "rastobj" - use sp::proj4string                              ####

#' @rdname get_proj4string
#' @method get_proj4string Raster
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.Raster <- function(object, abort = FALSE) {

  proj4string  <- sp::proj4string(object)
  proj4string <- check_proj4string(proj4string)
  return(proj4string)
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_crs                                 ####
#

#' @rdname get_proj4string
#' @method get_proj4string sf
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.sf <- function(object,
                               abort = FALSE) {

  proj4string  <- sf::st_crs(object)$proj4string
  proj4string <- check_proj4string(proj4string)
  return(proj4string)

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_proj4string
#' @method get_proj4string sfc
#' @importFrom rgdal checkCRSArgs
#' @export
get_proj4string.sfc <- function(object, abort = FALSE) {

  proj4string  <- sf::st_crs(object)$proj4string
  proj4string <- check_proj4string(proj4string)
  return(proj4string)

}

#   ____________________________________________________________________________
#   Method for "Spatial" object - use sp::proj4string(object)               ####
#

#' @rdname get_proj4string
#' @method get_proj4string Spatial
#' @export
get_proj4string.Spatial <- function(object, abort = FALSE) {

  proj4string  <- sp::proj4string(object)
  proj4string <- check_proj4string(proj4string)
  return(proj4string)

}
