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
#' obj                  <- read_vect(shp_file, as_sp = TRUE)
#' check_spatype(obj)
#'
#' # input is a `sf` object
#' obj                  <- read_vect(shp_file)
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
  if (!file.exists(object)) {
    stop("check_spatype --> ", object, "is not a valid filename. Aborting !")
  } else {

    # First, try checking if file is a vector if it has a common extension ----
    #@TODO update this list !
    vect_extensions <- c("shp", "kml", "gml", "dxf", "vpf")
    rast_extensions <- append(vect_extensions, toupper(vect_extensions))
    if (tools::file_ext(object) %in% vect_extensions) {
      vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))
      if (is.null(attr(vecttry, "status"))) {
        return("vectfile")
      } else {
        return("none")
      }
    }

    # Tehen, try checking if file is a raster if it has a common extension ----
    #@TODO update this list !
    rast_extensions <- c("tif", "tiff", "ecw", "hdf", "hdf4", "hdf5", "jp2",
                         "hdf4", "envi")
    rast_extensions <- append(rast_extensions, toupper(rast_extensions))
    if (tools::file_ext(object) %in% rast_extensions) {
      rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object), silent = TRUE))
      if (is.null(attr(rastry, "status"))) {
        return("rastfile")
      } else {
        return("none")
      }
    }
    else {
      # if unrecognized extension, try first to see if the file is a vector, if ----
      # it fails, try to see if it is a raster. If nothing "works", return "none"
      vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))
      if (is.null(attr(vecttry, "status"))) {
        return("vectfile")
      } else {
        rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object), silent = TRUE))
        if (is.null(attr(rastry, "status"))) {
          return("rastfile")
        } else {
          return("none")
        }
      }
    }
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

