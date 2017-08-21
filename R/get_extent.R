#' @title return the exten of a spatial object or file (with projection)
#' @description helper function used to retrieve the extent of a spatial object or file
#'   with the associated proj4string
#' @param object `character` corresponding to the name of a R object, or a filename
#' (full path)
#' @param proj4string (optional) `character` proj4string representing the projection of
#'  the input extent. It is needed only if 'object' does not include a
#'  projection (like [`extent`][raster::extent()] or [`bbox`][sp::bbox()]).
#' @param abort `logical` if TRUE, the function aborts in case no proj4string or
#'   invalid projstring is found, Default: FALSE
#' @return object of class `sprawlext`
#' @details return an object of class `sprawlext` with two slots:
#'   - $extent: `numeric (4)` extent of the object (xmin, ymin, xmanx, ymax)
#'   - $proj4string: `character` proj4string of the object
#' @examples
#' \dontrun{
#'  library(raster)
#'
#'  in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
#'  get_extent(in_rast)
#'
#'  in_rast <- raster::raster(in_rast)
#'  get_extent(in_rast)
#'
#'  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#'  get_extent(in_vect)
#'
#'  in_vect <- read_vect(in_vect)
#'  get_extent(in_vect)
#'
#' }
#' @rdname get_extent
#' @export
#' @importFrom rgdal ogrInfo
#' @importFrom gdalUtils gdalinfo
#' @importFrom raster extent
#' @importFrom sf st_bbox
#' @importFrom methods new
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
get_extent <- function(object,
                       proj4string = NULL,
                       abort = FALSE) {
  UseMethod("get_extent")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_extent
#' @method get_extent default
#' @export
get_extent.default  <- function(object,
                                abort = FALSE) {
  call <- match.call()
  if (abort == TRUE) {
    stop("get_extent --> ", call[[2]], " is not a valid vector or raster `R` object or
         filename. Aborting !")
  } else {
    warning("get_extent --> ", call[[2]], " is not a valid vector or raster `R` object or
            filename !")
  }
  }

#   ____________________________________________________________________________
#   Method for sprawlext (return input)                                      ####

#' @rdname get_extent
#' @method get_extent sprawlext
#' @export
get_extent.sprawlext  <- function(object,
                                  abort = FALSE) {
  return(object)
}

#   ____________________________________________________________________________
#   Method for extent (convert extent and proj4string in sprawlext)          ####

#' @rdname get_extent
#' @method get_extent extent
#' @export
get_extent.extent  <- function(object,
                               proj4string,
                               abort = FALSE) {
  coords        <- object[c(1,3,2,4)]
  proj4string   <- get_projstring(proj4string)
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_extent
#' @method get_extent character
#' @export
get_extent.character <- function(object,
                                 abort = FALSE) {
  call     <- match.call()
  obj_type <- get_spatype(object, abort = abort)

  if (obj_type %in% c("rastfile", "vectfile")) {

    if (obj_type == "vectfile") {
      coords      <- rgdal::ogrInfo(object, rgdal::ogrInfo(object)$layer)$extent
    } else {
      coords      <- as.numeric(gdalUtils::gdalinfo(object, raw_output = FALSE)$bbox)
    }

    names(coords) <- c("xmin", "ymin", "xmax", "ymax")
    proj4string   <- get_projstring(object)
    outext        <- methods::new("sprawlext",
                                  extent     = coords,
                                  proj4string = proj4string)
    return(outext)
  } else {
    if (abort == TRUE) {
      stop("get_extent --> `", call[[2]], "` is not a valid vector or raster `R` object
           or filename ! Aborting !")
    } else {
      warning("get_extent --> `", call[[2]], "` is not a valid vector or raster `R` object
              or filename ! ")
      return("none")
    }
  }
  }


#   ____________________________________________________________________________
#   Method for "rastobj" - use raster::extent                            ####

#' @rdname get_extent
#' @method get_extent Raster
#' @export
get_extent.Raster <- function(object,
                              abort = FALSE) {
  coords        <- raster::extent(object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  proj4string   <- get_projstring.Raster(object, abort = abort)
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_bbox                               ####
#

#' @rdname get_extent
#' @method get_extent sf
#' @export
get_extent.sf <- function(object,
                          abort = FALSE) {

  bbox          <- sf::st_bbox(object)
  coords        <- as.numeric(bbox)
  proj4string   <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_extent
#' @method get_extent sfc
#' @export
get_extent.sfc <- function(object,
                           abort = FALSE) {

  bbox          <- sf::st_bbox(object)
  coords        <- as.numeric(bbox)
  proj4string   <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "Spatial" object - use use raster::extent               ####
#

#' @rdname get_extent
#' @method get_extent Spatial
#' @export
get_extent.Spatial <- function(object,
                               abort = FALSE) {
  coords        <- raster::extent(object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  proj4string   <- get_projstring.Spatial(object, abort = abort)
  outext        <- methods::new("sprawlext",
                                extent     = coords,
                                proj4string = proj4string)
  return(outext)

}
