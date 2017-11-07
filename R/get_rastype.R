#' @title Check in the input is a `Raster` object or raster file
#' @description Check if a `R` object or a filename correspond to a valid `Raster`
#'   object, to a raster file or none of the above. Useful to detect which kind
#'   of input is passed to a function and abort / do something in the case of
#'   "wrong" input.
#' @param in_rast name of an `R` object, or `character` giving the full path
#'  to a spatial file
#' @param abort If TRUE, and `in_rast` is neither a raster object or
#'  filename, send an error message and abort, Default: TRUE
#' @return `character` equal to "rastfile" (if `in_rast` is a raster file),
#'   `rastobject` (if `in_rast` is a `R` raster object) or `NA`` if it is
#'   neither (unless `abort` == TRUE)
#' @examples
#'
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                         package = "sprawl.data")
#' get_rastype(in_rast)
#'
#' # input is a raster object
#' obj <- read_rast(in_rast)
#' get_spatype(obj)
#'
#' @rdname get_rastype
#' @export
#' @importFrom checkmate assert_file_exists
#' @importFrom rgdal GDALinfo
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

get_rastype  <- function(in_rast, abort = TRUE) {
  UseMethod("get_rastype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_rastype default
#' @export
get_rastype.default <- function(in_rast, abort = TRUE) {

  stop_message <- paste0("\"", deparse(substitute(in_rast)),
                         "\" is not a recognised raster object or filename.")
  if (abort) {
    stop(stop_message)
  } else {
    warning(stop_message)
    return(NA)
  }
}
#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_rastype character
#' @export
get_rastype.character <- function(in_rast, abort = TRUE) {

 # checkmate::assert_file_exists(in_rast, access = "r")

  rastry  <- suppressWarnings(try(rgdal::GDALinfo(in_rast),
                                  silent = TRUE))
  if (!is(rastry, "try-error")) {
    return("rastfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_rast)),
                           "\" is not a recognised raster filename.")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
}
#   __________________________________________________________________________
#   Method for "Raster"                                                  ####

#' @method get_rastype Raster
#' @export
get_rastype.Raster <- function(in_rast, abort = TRUE) {
  "rastobject"
}
