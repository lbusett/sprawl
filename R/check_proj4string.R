#' @title Check the validity of the input proj4string
#' @description helper function used to check that the input proj4string object
#'  is a valid string or a CRS object.
#' @param proj4string `character` or [`CRS`] corresponding to the
#'  proj4string to be checked
#' @param abort `logical` if TRUE, the function aborts in case no proj4string or
#'   invalid proj4string is found, Default: FALSE
#' @return `character` proj4string of the object or file
#' @details DETAILS
#'
#' @importFrom sp proj4string CRS
#' @importFrom rgdal checkCRSArgs CRSargs
#' @name check_proj4string
#' @rdname check_proj4string
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#'
#' @examples
#' check_proj4string("+init=epsg:32632")
#'
#' check_proj4string("example of invalid string")
#'
#' library(sp)
#' check_proj4string(CRS("+init=epsg:32632"))
#'
#' \dontrun{
#' library(raster)
#' in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
#' in_rast <- raster::raster(in_rast)
#' check_proj4string(in_rast@crs)
#' }

check_proj4string <- function(proj4string,
                              abort = FALSE) {
  UseMethod("check_proj4string")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname check_proj4string
#' @method check_proj4string default
#' @export
check_proj4string.default  <- function(proj4string,
                                       abort = FALSE) {
  call <- match.call()
  if (abort == TRUE) {
    stop("check_proj4string --> ", call[[2]], " is not a valid CRS object or ",
         "proj4string. Aborting!")
  } else {
    warning("check_proj4string --> ", call[[2]], " is not a valid CRS object ",
            "or proj4string.")
  }
}

#   ____________________________________________________________________________
#   Method for character - check that it is a valid proj4string             ####

#' @rdname check_proj4string
#' @method check_proj4string character
#' @export
#' @importFrom rgdal checkCRSArgs
check_proj4string.character  <- function(proj4string,
                                         abort = FALSE) {
  if (rgdal::checkCRSArgs(proj4string)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("check_proj4string --> Invalid proj4string detected! Aborting!")
    } else {
      warning("check_proj4string --> Invalid proj4string detected!")
      return("invalid")
    }
  } else {
    return(rgdal::checkCRSArgs(proj4string)[[2]])
  }
}

#   ____________________________________________________________________________
#   Method for CRS - get proj4string                                        ####

#' @rdname check_proj4string
#' @method check_proj4string CRS
#' @export
check_proj4string.CRS  <- function(proj4string,
                                   abort = FALSE) {
  return(CRSargs(proj4string))
}
