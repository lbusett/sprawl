#' @title return the extent of a spatial object or file (with projection)
#' @description helper function used to retrieve the extent of a spatial object
#'  or file with the associated proj4string
#' @param object `character` corresponding to the name of a R object, or a
#'  filename (full path)
#' @param abort `logical` if TRUE, the function aborts in case no proj4string or
#'   invalid projstring is found, Default: FALSE
#' @return object of class `sprawl_ext`
#' @details return an object of class `sprawl_ext` with two slots:
#'   - @extent: `numeric (4)` extent of the object (xmin, ymin, xmax, ymax)
#'   - @projstring: `character` proj4string of the object
#' @examples
#' \dontrun{
#'  library(raster)
#'
#'  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'    package = "sprawl.data")
#'  get_extent(in_rast)
#'
#'  in_rast <- raster::raster(in_rast)
#'  get_extent(in_rast)
#'
#'  in_vect <- system.file("extdata/shapes","lc_polys.shp",
#'    package = "sprawl.data")
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
                       abort = FALSE) {
  UseMethod("get_extent")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname get_extent
#' @method get_extent default
#' @export
get_extent.default  <- function(object, abort = FALSE) {

  call <- match.call()
  if (abort == TRUE) {
    stop("get_extent --> ", call[[2]], " is not a valid vector or ",
         "raster `R` object or filename. Aborting !")
  } else {
    warning("get_extent --> ", call[[2]], " is not a valid vector ",
            "or raster `R` object or filename !")
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @rdname get_extent
#' @method get_extent character
#' @export
get_extent.character <- function(object, abort = FALSE) {

  call     <- match.call()
  obj_type <- get_spatype(object, abort = abort)

  if (obj_type %in% c("rastfile", "vectfile")) {

    if (obj_type == "vectfile") {
      coords <- rgdal::ogrInfo(object, rgdal::ogrInfo(object)$layer)$extent
    } else {
      coords <- as.numeric(gdalUtils::gdalinfo(object, raw_output = FALSE)$bbox)
    }

    names(coords) <- c("xmin", "ymin", "xmax", "ymax")
    projstring    <- get_projstring(object)
    outext        <- methods::new("sprawlext",
                                  extent     = coords,
                                  projstring = projstring)
    return(outext)
  } else {
    if (abort == TRUE) {
      stop("get_extent --> `", call[[2]], "` is not a valid vector ",
           "or raster `R` object or filename ! Aborting !")
    } else {
      warning("get_extent --> `", call[[2]], "` is not a valid ",
              "vector or raster `R` object or filename ! ")
      return("none")
    }
  }
}

#   ____________________________________________________________________________
#   Method for "rastobj" - use raster::extent                               ####

#' @rdname get_extent
#' @method get_extent Raster
#' @export
get_extent.Raster <- function(object, abort = FALSE) {

  coords        <- raster::extent(object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  projstring    <- get_projstring(object, abort)
  outext        <- methods::new("sprawlext", extent = coords,
                                projstring = projstring)
  return(outext)
}

#   ____________________________________________________________________________
#   Method for "sf" object - use sf::st_bbox                                ####
#

#' @rdname get_extent
#' @method get_extent sf
#' @export
get_extent.sf <- function(object, abort = FALSE) {

  bbox          <- sf::st_bbox(object)
  coords        <- as.numeric(bbox)
  projstring    <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext", extent     = coords,
                                projstring = projstring)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "sf" object - sf::st_crs                                     ####

#' @rdname get_extent
#' @method get_extent sfc
#' @export
get_extent.sfc <- function(object, abort = FALSE) {

  bbox          <- sf::st_bbox(object)
  coords        <- as.numeric(bbox)
  projstring    <- attributes(bbox)[[3]]$proj4string
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  outext        <- methods::new("sprawlext", extent     = coords,
                                projstring = projstring)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "Spatial" object - use use raster::extent               ####
#

#' @rdname get_extent
#' @method get_extent Spatial
#' @export
get_extent.Spatial <- function(object, abort = FALSE) {

  coords        <- raster::extent(object)[c(1,3,2,4)]
  names(coords) <- c("xmin", "ymin", "xmax", "ymax")
  projstring    <- get_projstring.Spatial(object, abort = abort)
  outext        <- methods::new("sprawlext", extent     = coords,
                                projstring = projstring)
  return(outext)

}

#   ____________________________________________________________________________
#   Method for "sprawlext" object - do nothing                              s####
#

#' @rdname get_extent
#' @method get_extent Spatial
#' @export
get_extent.sprawlext <- function(object, abort = FALSE) {
  return(object)

}
