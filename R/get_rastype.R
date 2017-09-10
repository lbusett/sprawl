#' @title check the "spatial type" of an object or file
#' @rdname get_spatype
#' @export
#' @importFrom checkmate assert_file_exists
#' @importFrom rgdal GDALinfo

get_rastype  <- function(in_object, abort = TRUE) {
  UseMethod("get_rastype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_rastype default
#' @export
get_rastype.default <- function(in_object, abort = TRUE) {

  stop_message <- paste0("\"", deparse(substitute(in_object)),
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
get_rastype.character <- function(in_object, abort = TRUE) {

  checkmate::assert_file_exists(in_object, access = "r")

  rastry  <- suppressWarnings(try(rgdal::GDALinfo(in_object),
                                  silent = TRUE))
  if (!is(rastry, "try-error")) {
    return("rastfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_object)),
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
get_rastype.Raster <- function(in_object, abort = TRUE) {
  "rastobject"
}
