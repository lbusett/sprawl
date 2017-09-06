#' @title check the "spatial type" of a vector object or file
#' @description accessory function to check if an object passed to the function
#'   corresponds to a `*Spatial` Object, a `sf` object, or a valid vector file
#' @param object either a `R` object or a `character` string pointing to a vector
#'   or raster layer
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#' @rdname get_vectype
#' @export
#' @importFrom gdalUtils ogrinfo gdalinfo
#' @examples \dontrun{
#' library(sprawl.data)
#'
#' # input is a shapefile
#' in_vect <- system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data")
#' get_vectype(in_vect)
#'
#' # input is a `sp` object
#' obj <- read_vect(in_vect, as_sp = TRUE)
#' get_vectype(obj)
#'
#' # input is a `sf` object
#' obj <- read_vect(in_vect)
#' get_vectype(obj)
#' }
#'

get_vectype  <- function(object) {
  UseMethod("get_vectype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @rdname get_vectype
#' @method get_vectype default
#' @export
get_vectype.default <- function(in_object) {
  call = match.call()
    stop("get_vectype --> ", call[[2]], " is not a R object or filename. ",
         "Aborting !")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_vectype
#' @method get_vectype character
#' @export
get_vectype.character <- function(in_object) {
  call <- match.call()
  checkmate::assert_file_exists(in_object, access = "r")
  vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(in_object, al = TRUE,
                                                     so = TRUE,
                                                     verbose = FALSE,
                                                     q = TRUE),
                                  silent = TRUE))
  if (is.null(attr(vecttry, "status"))) {
    return("vectfile")
  } else {
    stop("get_vectype --> ", call[[2]], " is not a valid vector file. ",
         "Aborting !")
  }
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @rdname get_vectype
#' @method get_vectype sf
#' @export
get_vectype.sf <- function(object) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

#' @rdname get_vectype
#' @method get_vectype sfc
#' @export
get_vectype.sfc  <- function(object) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @rdname get_vectype
#' @method get_vectype Spatial
#' @export
get_vectype.Spatial <- function(object) {
  "spobject"
}

#   ____________________________________________________________________________
#   Method for "sprawlext"                                                    ####

#' @rdname get_vectype
#' @method get_vectype Spatial
#' @export
get_vectype.sprawlext <- function(object) {
  "sprawlext"
}

