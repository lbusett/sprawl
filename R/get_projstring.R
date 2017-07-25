#' @title return the proj4string of a spatial object or file
#' @description helper function used to extract the proj4string of "R" spatial objects
#'   or of raster or vector files
#' @param object `character` corresponding to the name of a R object, or a filename
#' (full path)
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
get_projstring <- function(object) {
  UseMethod("get_projstring")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname
#' @method get_projstring default
#' @export
get_projstring.default  <- function(object) {
  stop("get_projstring --> ", object, " is not a valid vector or raster `R` object or
   filename. Aborting !")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_projstring
#' @method get_projstring character
#' @export
get_projstring.character <- function(object) {

  obj_type <- check_spatype(object)
  dplyr::case_when(
    obj_type %in% c("rastfile", "vectfile") ~
      return(as.character(gdalUtils::gdalsrsinfo(object, as.CRS = TRUE))),

    obj_type == "none" ~
      stop("get_projstring --> ", object, "is not a recognized vector or raster file.
           Aborting !")
  )
}

#   ____________________________________________________________________________
#   Method for "rastobj" - use sp::proj4string                              ####

#' @rdname get_projstring
#' @method get_projstring Raster
#' @export
get_projstring.Raster <- function(object) {

  return(sp::proj4string(object))

}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_crs                                 ####
#

#' @rdname get_projstring
#' @method get_projstring sf
#' @export
get_projstring.sf <- function(object) {

  return(sf::st_crs(object)$proj4string)

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_projstring
#' @method get_projstring sfc
#' @export
get_projstring.sfc <- function(object) {

  return(sf::st_crs(object)$proj4string)

}


#   ____________________________________________________________________________
#   Method for "Spatial" object - use sp::proj4string(object)               ####
#

#' @rdname get_projstring
#' @method get_projstring Spatial
#' @export
get_projstring.Spatial <- function(object) {

  return(sp::proj4string(object))

}
