#' @title check the "spatial type" of an object or file
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

check_spatype  <- function(object,
                           abort = FALSE) {
  UseMethod("check_spatype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @rdname check_spatype
#' @method check_spatype default
#' @export
check_spatype.default   <- function(object,
                                    abort = FALSE) {

  if (abort == TRUE) {
    stop("check_spatype --> `object` is not a valid R object or string. Aborting !")
  } else {
    warning("check_spatype --> `object` is not a valid R object or string.")
    return("none")
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname check_spatype
#' @method check_spatype Spatial
#' @export
check_spatype.character <- function(object,
                                    abort = FALSE) {

  if (!file.exists(object)) {
    if (abort == TRUE) {
      stop("check_spatype --> `object` is not a valid filename. Aborting !")
    } else {
      warning("check_spatype --> `object` is not a valid filename.")
      return("none")
    }
  } else {

    # First, try checking if file is a vector if it has a common extension ----
    #@TODO update this list !
    vect_extensions <- c("shp", "kml", "gml", "dxf", "vpf")
    vect_extensions <- append(vect_extensions, toupper(vect_extensions))
    if (tools::file_ext(object) %in% vect_extensions) {
      vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))
      if (is.null(attr(vecttry, "status"))) {
        return("vectfile")
      } else {
        if (abort == TRUE) {
          stop("check_spatype --> `object` is not a valid vector file. Aborting !")
        } else {
          warning("check_spatype --> `object` is not a valid vector file. Aborting !")
          return("none")
        }
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
        if (abort == TRUE) {
          stop("check_spatype --> `object` is not a valid raster file. Aborting !")
        } else {
          warning("check_spatype --> `object` is not a valid raster file. Aborting !")
          return("none")
        }
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
          if (abort == TRUE) {
            stop("check_spatype --> `object` is not a valid raster or vector file. Aborting !")
          } else {
            warning("check_spatype --> `object` is not a valid raster or vector file. Aborting !")
            return("none")
          }
        }
      }
    }
  }
}

#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

#' @rdname check_spatype
#' @method check_spatype Raster
#' @export
check_spatype.Raster    <- function(object) {
  "rastobject"
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @rdname check_spatype
#' @method check_spatype sf
#' @export
check_spatype.sf        <- function(object) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

#' @rdname check_spatype
#' @method check_spatype sfc
#' @export
check_spatype.sfc        <- function(object) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @rdname check_spatype
#' @method check_spatype Spatial
#' @export
check_spatype.Spatial   <- function(object) {
  "spobject"
}
