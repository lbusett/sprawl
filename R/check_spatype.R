#' @title check_spatype
#' @description accessory function to check if an object passed to the function corresponds to
#' a `*Spatial` Object, a `sf` object, a R `raster` object, a file corresponding to a vector,
#' or a file corresponding to a raster
#' @param object either a `R` object or a `character` string pointing to a vector or raster layer
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#' @rdname check_spatype
#' @export
#' @importFrom gdalUtils ogrinfo gdalinfo
#' @examples \dontrun{
#' library(sprawl.data)
#' # input is a raster file
#' check_spatype(system.file("extdata","testrast.tif", package = "sprawl.data"))
#'
#' # input is a shapefile
#' shp_file             <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#' check_spatype(shp_file)
#'
#' # input is a `sp` object
#' obj                  <- read_shape(shp_file, as_sp = TRUE)
#' check_spatype(obj)
#'
#' # input is a `sf` object
#' obj                  <- read_shape(shp_file)
#' check_spatype(obj)
#' }
#'
# check_spatype_met     <- function(object) {
#   UseMethod("check_spatype", object)
# }
#
# check_spatype.default <- function(object) {
#   stop("none")
# }
#
#

check_spatype  <- function(object) {
  UseMethod("check_spatype")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

check_spatype.default   <- function(object) {
  "none"
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

check_spatype.character <- function(object) {
  if (file.exists(object)) {
    vecttry             <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))
    if (is.null(attr(vecttry, "status"))) {
      "vectfile"
    } else {
      rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object), silent = TRUE))
      if (is.null(attr(rastry, "status"))) {
        "rastfile"
      } else {
        "none"
      }
    }
  } else {
    "none"
  }
}


#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

check_spatype.Raster    <- function(object) {
  "rastobject"
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

check_spatype.sf        <- function(object) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

check_spatype.sfc        <- function(object) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

check_spatype.Spatial   <- function(object) {
  "spobject"
}

