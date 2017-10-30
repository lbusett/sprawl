#' @title check the "spatial type" of an object or file
#' @description Check if a `R` object or a filename correspond to a valid vector
#'   object, to a vector file or none of the above. Useful to detect which kind
#'   of input is passed to a function and abort / do something in the case of
#'   "wrong" input.
#' @param in_vect name of an `R` object, or `character` giving the full path
#'  to a vector file
#' @param abort If TRUE, and `in_vect` is neither a spatial object or
#'  filename, send an error message and abort, Default: TRUE
#' @return `character` equal to "vectfile" (if `in_vect` is a raster file),
#'   `spobject` (if `in_vect` is of class `spatial`), `sfobject` (if `in_vect`
#'   is of class `sf`), or `NA` if it is neither (unless `abort` == TRUE)
#' @rdname get_vectype
#' @export
#' @examples
#' # input is a shapefile
#' in_vect <- system.file("extdata/shapes","lc_polys.shp",
#'            package = "sprawl.data")
#' get_vectype(in_vect)
#'
#' # input is a `sp` object
#' obj <- read_vect(in_vect, as_sp = TRUE)
#' get_vectype(obj)
#'
#' # input is a `sf` object
#' obj <- read_vect(in_vect)
#' get_vectype(obj)
#'
#' # input is a `sprawlext` object
#' obj <- get_extent(in_vect)
#' get_vectype(obj)
#' @importFrom gdalUtils ogrinfo
#' @importFrom checkmate assert_file_exists

get_vectype  <- function(in_vect, abort = TRUE) {
  UseMethod("get_vectype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_vectype default
#' @export
get_vectype.default <- function(in_vect, abort = TRUE) {
  stop_message <- paste0("\"", deparse(substitute(in_vect)),
                         "\" is not a recognised vector object or filename.")
  if (abort) {
    stop(stop_message)
  } else {
    warning(stop_message)
    return(NA)
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_vectype character
#' @importFrom gdalUtils ogrinfo
#' @importFrom checkmate assert_file_exists
#' @export
get_vectype.character <- function(in_vect, abort = TRUE) {
  checkmate::assert_file_exists(in_vect, access = "r")
  vecttry <- suppressWarnings(
    try(gdalUtils::ogrinfo(in_vect, al = TRUE,
                           so = TRUE,
                           verbose = FALSE,
                           q = TRUE),
        silent = TRUE)
  )
  if (is.null(attr(vecttry, "status"))) {
    return("vectfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_vect)),
                           "\" is not a recognised vector filename.")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @method get_vectype sf
#' @export
get_vectype.sf <- function(in_vect, abort = TRUE) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                       ####

#' @method get_vectype sfc
#' @export
get_vectype.sfc  <- function(in_vect, abort = TRUE) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @method get_vectype Spatial
#' @export
get_vectype.Spatial <- function(in_vect, abort = TRUE) {
  "spobject"
}

#   ____________________________________________________________________________
#   Method for "sprawlext"                                                 ####

#' @method get_vectype Spatial
#' @export
get_vectype.sprawlext <- function(in_vect, abort = TRUE) {
  "sprawlext"
}
