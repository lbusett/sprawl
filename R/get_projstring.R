#' @title return the proj4string of a spatial object or file
#' @description helper function used to extract the proj4string of "R" spatial objects
#'   or of raster or vector files
#' @param object `character` corresponding to the name of a R object, or a filename
#' (full path)
#' @param abort `logical` if TRUE, the function aborts in case no proj4string or
#'   invalid projstring is found, Default: FALSE
#' @return `character` proj4string of the object or file
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(raster)
#'
#'  in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
#'  get_projstring(in_rast)
#'
#'  in_rast <- raster::raster(in_rast)
#'  get_projstring(in_rast)
#'
#'  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#'  get_projstring(in_vect)
#'
#'  in_vect <- read_vect(in_vect)
#'  get_projstring(in_vect)
#'
#'  }
#' }
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom sp proj4string
#' @importFrom sf st_crs
#' @rdname get_projstring
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
get_projstring <- function(object,
                           abort = FALSE) {
  UseMethod("get_projstring")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_projstring
#' @method get_projstring default
#' @export
get_projstring.default  <- function(object, abort = FALSE) {
  if (abort == TRUE) {
    stop("get_projstring --> `object` is not a valid vector or raster `R` object or
   filename. Aborting !")
  } else {
    warning("get_projstring --> `object` is not a valid vector or raster `R` object or
   filename. Aborting !")
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_projstring
#' @method get_projstring character
#' @importFrom rgdal checkCRSArgs
#' @export
get_projstring.character <- function(object, abort = FALSE) {

  obj_type <- check_spatype(object)

  if (obj_type %in% c("rastfile", "vectfile")) {

    projstring  <- as.character(gdalUtils::gdalsrsinfo(object, as.CRS = TRUE))
    if (rgdal::checkCRSArgs(projstring)[[1]] == FALSE) {
      if (abort == TRUE) {
        stop("get_projstring --> Invalid proj4string detected ! Aborting !")
      } else {
        warning("get_projstring --> Invalid proj4string detected ! Aborting !")
        return("invalid")
      }
    } else {
      return(projstring)
    }
  } else {
    if (abort == TRUE) {
      stop("get_projstring --> `object` is not a valid raster or vector file, Aborting !")
    } else {
      warning("get_projstring --> `object` is not a valid raster or vector file, Aborting !")
      return("none")
    }
  }
}


#   ____________________________________________________________________________
#   Method for "rastobj" - use sp::proj4string                              ####

#' @rdname get_projstring
#' @method get_projstring Raster
#' @importFrom rgdal checkCRSArgs
#' @export
get_projstring.Raster <- function(object, abort = FALSE) {

  projstring  <- sp::proj4string(object)
  if (rgdal::checkCRSArgs(projstring)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("get_projstring --> Invalid proj4string detected ! Aborting !")
    } else {
      warning("get_projstring --> Invalid proj4string detected ! Aborting !")
      return("invalid")
    }
  } else {
    return(projstring)
  }
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_crs                                 ####
#

#' @rdname get_projstring
#' @method get_projstring sf
#' @importFrom rgdal checkCRSArgs
#' @export
get_projstring.sf <- function(object, abort = FALSE) {

  projstring  <- sf::st_crs(object)$proj4string
  if (rgdal::checkCRSArgs(projstring)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("get_projstring --> Invalid proj4string detected ! Aborting !")
    } else {
      warning("get_projstring --> Invalid proj4string detected ! Aborting !")
      return("invalid")
    }
  } else {
    return(projstring)
  }

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_projstring
#' @method get_projstring sfc
#' @importFrom rgdal checkCRSArgs
#' @export
get_projstring.sfc <- function(object, abort = FALSE) {

  projstring  <- sf::st_crs(object)$proj4string
  if (rgdal::checkCRSArgs(projstring)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("get_projstring --> Invalid proj4string detected ! Aborting !")
    } else {
      warning("get_projstring --> Invalid proj4string detected ! Aborting !")
      return("invalid")
    }
  } else {
    return(projstring)
  }

}


#   ____________________________________________________________________________
#   Method for "Spatial" object - use sp::proj4string(object)               ####
#

#' @rdname get_projstring
#' @method get_projstring Spatial
#' @export
get_projstring.Spatial <- function(object, abort = FALSE) {

  projstring  <- sp::proj4string(object)
  if (rgdal::checkCRSArgs(projstring)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("get_projstring --> Invalid proj4string detected ! Aborting !")
    } else {
      warning("get_projstring --> Invalid proj4string detected ! Aborting !")
      return("invalid")
    }
  } else {
    return(projstring)
  }

}
