#' @title return the extent of a spatial object or file (with projection)
#' @description helper function used to retrieve the extent of a spatial object
#'  or file, and the associated proj4string
#' @param in_object `character` corresponding to the name of a R object, or a
#'  filename (full path)
#' @param proj4string (optional) `character` proj4string representing the projection of
#'  the input extent. It is needed only if `in_object` does not include a
#'  projection (like [`extent`] or [`bbox`]).
#' @param abort `logical` if TRUE, the function aborts in case valid a extent can
#'  not be retrieved, otherwise, it issues a Warning and returns `NA`,
#'  Default: FALSE
#' @return object of class `sprawlext`
#' @details return an object of class `sprawlext` with two slots:
#'   - $extent: `numeric (4)` extent of the object (xmin, ymin, xmax, ymax)
#'   - $proj4string: `character` proj4string of the object
#'   (if an extent can not be retrieved from `in_object`)
#' @name get_extent
#' @rdname get_extent
#' @export
#' @importFrom rgdal ogrInfo
#' @importFrom gdalUtils gdalinfo
#' @importFrom raster extent
#' @importFrom sf st_bbox
#' @importFrom methods new
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @examples
#'
#' library(raster)
#'
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'   package = "sprawl.data")
#' get_extent(in_rast)
#'
#' in_rast <- raster::raster(in_rast)
#' get_extent(in_rast)
#'
#' in_vect <- system.file("extdata/shapes","lc_polys.shp",
#'   package = "sprawl.data")
#' get_extent(in_vect)
#'
#' in_vect <- read_vect(in_vect)
#' get_extent(in_vect)
#'
get_extent <- function(in_object,
                       proj4string = NULL,
                       abort = FALSE) {
  UseMethod("get_extent")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_extent
#' @method get_extent default
#' @export
get_extent.default  <- function(in_object,
                                proj4string = NULL,
                                abort = FALSE) {
  call <- match.call()
  if (abort == TRUE) {
    stop("get_extent -> ", call[[2]], " is not a valid vector or ",
         "raster `R` object or filename. Aborting!")
  } else {
    warning("get_extent -> ", call[[2]], " is not a valid vector ",
            "or raster `R` object or filename!")
  }
}

#   ____________________________________________________________________________
#   Method for sprawlext (return input)                                      ####

#' @rdname get_extent
#' @method get_extent sprawlext
#' @export
get_extent.sprawlext <- function(in_object,
                                  proj4string = NULL,
                                  abort = FALSE) {
  return(in_object)
}

#   ____________________________________________________________________________
#   Method for Extent (convert extent and proj4string in sprawlext)          ####

#' @rdname get_extent
#' @method get_extent Extent
#' @export
get_extent.Extent  <- function(in_object,
                               proj4string,
                               abort = FALSE) {
  assertthat::assert_that(class("proj4string") == "character")
  coords        <- in_object[c(1,3,2,4)]
  proj4string   <- check_proj4string(proj4string, abort = TRUE)
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent      = coords,
                                proj4string = proj4string)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for bbox   (convert bbox   and proj4string in sprawlext)          ####

#' @rdname get_extent
#' @method get_extent matrix
#' @export
get_extent.matrix  <- function(in_object,
                               proj4string,
                               abort = FALSE) {
  # check matrix
  if (any(dim(in_object) != c(2,2))) {
    if (abort == TRUE) {
      stop("get_extent -> `", call[[2]], "` is not a valid bbox! Aborting!")
    } else {
      warning("get_extent -> `", call[[2]], "` is not a valid bbox! ")
      return(NA)
    }
  }
  coords        <- as.numeric(in_object)
  proj4string   <- check_proj4string(proj4string, abort = TRUE)
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent      = coords,
                                proj4string = proj4string)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_extent
#' @method get_extent character
#' @export
get_extent.character <- function(in_object,
                                 proj4string = NULL,
                                 abort = FALSE) {

  call     <- match.call()

  obj_type <- try(get_rastype(in_object), silent = TRUE)
  if (class(obj_type) == "try-error") {
    obj_type <- try(get_vectype(in_object), silent = TRUE)
  }
  if (class(obj_type) == "try-error") {
    if (abort == TRUE) {
      stop("get_extent -> `", call[[2]], "` is not a valid vector ",
           "or raster `R` object or filename! Aborting!")
    } else {
      warning("get_extent -> `", call[[2]], "` is not a valid ",
              "vector or raster `R` object or filename!")
      return(NA)
    }
  }


  if (obj_type %in% c("rastfile", "vectfile")) {

    if (obj_type == "vectfile") {
      coords <- rgdal::ogrInfo(in_object, rgdal::ogrInfo(in_object)$layer)$extent
    } else {
      coords <- as.numeric(gdalUtils::gdalinfo(in_object, raw_output = FALSE)$bbox)
    }

    names(coords) <- c("xmin", "ymin", "xmax", "ymax")
    proj4string   <- get_proj4string(in_object)
    outext        <- methods::new("sprawlext",
                                  extent     = coords,
                                  proj4string = proj4string)
    return(outext)
  } else {
    if (abort == TRUE) {
      stop("get_extent -> `", call[[2]], "` is not a valid vector ",
           "or raster `R` object or filename! Aborting!")
    } else {
      warning("get_extent -> `", call[[2]], "` is not a valid ",
              "vector or raster `R` object or filename!")
      return(NA)
    }
  }
  }

#   ____________________________________________________________________________
#   Method for "rastobj" - use raster::extent                               ####

#' @rdname get_extent
#' @method get_extent Raster
#' @export
get_extent.Raster <- function(in_object,
                              proj4string = NULL,
                              abort = FALSE) {

  coords        <- raster::extent(in_object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  proj4string   <- get_proj4string.Raster(in_object)
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_bbox                                ####
#

#' @rdname get_extent
#' @method get_extent sf
#' @export
get_extent.sf <- function(in_object,
                          proj4string = NULL,
                          abort = FALSE) {

  bbox          <- sf::st_bbox(in_object)
  coords        <- as.numeric(bbox)
  proj4string   <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "sf" in_object - sf::st_crs                                     ####

#' @rdname get_extent
#' @method get_extent sfc
#' @export
get_extent.sfc <- function(in_object,
                           proj4string = NULL,
                           abort = FALSE) {

  bbox          <- sf::st_bbox(in_object)
  coords        <- as.numeric(bbox)
  proj4string   <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "Spatial" object - use raster::extent               ####
#

#' @rdname get_extent
#' @method get_extent Spatial
#' @export
get_extent.Spatial <- function(in_object,
                               proj4string = NULL,
                               abort = FALSE) {

  coords        <- raster::extent(in_object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  proj4string   <- get_proj4string.Spatial(in_object)
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "sprawlext" object - do nothing                              ####
#

#' @rdname get_extent
#' @method get_extent Spatial
#' @export
get_extent.sprawlext <- function(in_object,
                                 proj4string = NULL,
                                 abort = FALSE) {
  return(in_object)

}
