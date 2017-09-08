#' @title check the "spatial type" of an object or file
#' @rdname get_spatype
#' @export
#' @importFrom rgdal GDALinfo

get_rastype  <- function(in_object) {
  UseMethod("get_rastype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_rastype default
#' @export
get_rastype.default <- function(in_object) {

  stop("get_vectype --> ", deparse(substitute(in_object)),
       " is not a R object or filename. ",
       "Aborting!")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_rastype character
#' @export
get_rastype.character <- function(in_object) {

  checkmate::assert_file_exists(in_object, access = "r")

  rastry  <- suppressWarnings(try(rgdal::GDALinfo(in_object),
                                  silent = TRUE))
  if (!is(rastry, "try-error")) {
    return("rastfile")
  } else {
    stop("get_vectype --> ", deparse(substitute(in_object)),
         " is not a valid vector file. ",
         "Aborting!")
  }
}

#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

#' @method get_rastype Raster
#' @export
get_rastype.Raster <- function(in_object) {
  "rastobject"
}
