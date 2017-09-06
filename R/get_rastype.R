#' @title check the "spatial type" of an object or file
#' @description accessory function to check if an object passed to the function corresponds to
#'   a `*Spatial` Object, a `sf` object, a R `raster` object, a file corresponding to a vector,
#'   or a file corresponding to a raster
#' @param in_object either a `R` object or a `character` string pointing to a vector or raster layer
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#' @rdname get_rastype
#' @export
#' @importFrom gdalUtils ogrinfo gdalinfo
#' @examples \dontrun{
#' library(sprawl.data)
#' # input is a raster file
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif", package = "sprawl.data")
#' get_rastype(in_rast)
#' }

get_rastype  <- function(in_object) {
  UseMethod("get_rastype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @rdname get_rastype
#' @method get_rastype default
#' @export
get_rastype.default <- function(in_object) {

  call = match.call()
  stop("get_vectype --> ", call[[2]], " is not a R object or filename. ",
       "Aborting !")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_rastype
#' @method get_rastype character
#' @export
get_rastype.character <- function(in_object) {

  call <- match.call()
  checkmate::assert_file_exists(in_object, access = "r")

  rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(in_object),
                                  silent = TRUE))
  if (is.null(attr(rastry, "status"))) {
    return("rastfile")
  } else {
    stop("get_vectype --> ", call[[2]], " is not a valid vector file. ",
         "Aborting !")
  }
}

#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

#' @rdname get_rastype
#' @method get_rastype Raster
#' @export
get_rastype.Raster <- function(in_object) {
  "rastobject"
}
