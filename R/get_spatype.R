#' @title check the "spatial type" of an object or file
#' @description accessory function to check if an object passed to the function corresponds to
#'   a `*Spatial` Object, a `sf` object, a R `raster` object, a file corresponding to a vector,
#'   or a file corresponding to a raster
#' @param object either a `R` object or a `character` string pointing to a vector or raster layer
#' @param abort `logical` if TRUE the function aborts if `object` is not recognized as an
#'   R spatial file or valid vector or raster file
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#' @rdname get_spatype
#' @export
#' @importFrom gdalUtils ogrinfo gdalinfo
#' @examples \dontrun{
#' library(sprawl.data)
#' # input is a raster file
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif", package = "sprawl.data")
#' get_spatype(in_rast)
#'
#' # input is a shapefile
#' in_vect <- system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data")
#' get_spatype(in_vect)
#'
#' # input is a `sp` object
#' obj <- read_vect(in_vect, as_sp = TRUE)
#' get_spatype(obj)
#'
#' # input is a `sf` object
#' obj <- read_vect(in_vect)
#' get_spatype(obj)
#' }
#'
# get_spatype_met <- function(object) {
#   UseMethod("get_spatype", object)
# }
#
# get_spatype.default <- function(object) {
#   stop("none")
# }
#
#

get_spatype  <- function(object,
                         abort = FALSE) {
  UseMethod("get_spatype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @rdname get_spatype
#' @method get_spatype default
#' @export
get_spatype.default <- function(object,
                                abort = FALSE) {

  if (abort == TRUE) {
    stop("get_spatype --> `object` is not a valid R object or string. ",
         "Aborting !")
  } else {
    warning("get_spatype --> `object` is not a valid R object or string.")
    return("none")
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_spatype
#' @method get_spatype character
#' @export
get_spatype.character <- function(object,
                                  abort = FALSE) {

  if (!file.exists(object)) {
    if (abort == TRUE) {
      stop("get_spatype --> `object` is not a valid filename. Aborting !")
    } else {
      warning("get_spatype --> `object` is not a valid filename.")
      return("none")
    }
  } else {

    # First, try checking if file is a vector if it has a common extension ----
    #@TODO update this list !
    vect_extensions <- c("shp", "kml", "gml", "dxf", "vpf")
    vect_extensions <- append(vect_extensions, toupper(vect_extensions))
    if (tools::file_ext(object) %in% vect_extensions) {
      vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object,
                                                         al = TRUE, so = TRUE),
                                      silent = TRUE))
      if (is.null(attr(vecttry, "status"))) {
        return("vectfile")
      } else {
        if (abort == TRUE) {
          stop("get_spatype --> `object` is not a valid vector file. ",
               "Aborting !")
        } else {
          warning("get_spatype --> `object` is not a valid vector file. ",
                  "Aborting !")
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
      rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object),
                                      silent = TRUE))
      if (is.null(attr(rastry, "status"))) {
        return("rastfile")
      } else {
        if (abort == TRUE) {
          stop("get_spatype --> `object` is not a valid raster file. ",
               "Aborting !")
        } else {
          warning("get_spatype --> `object` is not a valid raster file. ",
                  "Aborting!")
          return("none")
        }
      }
    }
    else {

      # if unrecognized extension, try first to see if the file is a vector, if
      # it fails try to see if it is a raster. If nothing "works", return "none"
      vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE,
                                                         so = TRUE),
                                      silent = TRUE))
      if (is.null(attr(vecttry, "status"))) {
        return("vectfile")
      } else {
        rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object),
                                        silent = TRUE))
        if (is.null(attr(rastry, "status"))) {
          return("rastfile")
        } else {
          if (abort == TRUE) {
            stop("get_spatype --> `object` is not a valid rspatial file. ",
                 "Aborting !")
          } else {
            warning("get_spatype --> `object` is not a valid rspatial file. ",
                    "Aborting !")
            return("none")
          }
        }
      }
    }
  }
}

#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

#' @rdname get_spatype
#' @method get_spatype Raster
#' @export
get_spatype.Raster <- function(object,
                               abort = FALSE) {
  "rastobject"
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @rdname get_spatype
#' @method get_spatype sf
#' @export
get_spatype.sf <- function(object,
                           abort = FALSE) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

#' @rdname get_spatype
#' @method get_spatype sfc
#' @export
get_spatype.sfc  <- function(object,
                             abort = FALSE) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @rdname get_spatype
#' @method get_spatype Spatial
#' @export
get_spatype.Spatial <- function(object,
                                abort = FALSE) {
  "spobject"
}
