#' @title check the "spatial type" of an object or file
#' @description accessory function to check if an object passed to the function corresponds to
#'   a `*Spatial` Object, a `sf` object, a R `raster` object, a file corresponding to a vector,
#'   or a file corresponding to a raster
#' @param in_object either a `R` object or a `character` string pointing to a vector or raster layer
#' @param abort (only in `get_spatype`) `logical` if TRUE the function aborts if `object` is not recognized as an
#'   R spatial file or valid vector or raster file; if FALSE, a warning is shown.
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile*, *rastfile* or *sprawlext*
#' @name get_spatype
#' @rdname get_spatype
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @examples \dontrun{
#' library(sprawl.data)
#' # input is a raster file
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif", package = "sprawl.data")
#' get_spatype(in_rast)
#'
#' # input is a raster file
#' obj <- read_rast(in_rast)
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
#'
#' # input is a `sprawlext` object
#' obj <- get_extent(in_vect)
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

get_spatype  <- function(in_object,
                         abort = TRUE) {

  obj_type <- try(get_rastype(in_object), silent = TRUE)
  if (class(obj_type) == "try-error") {
    obj_type <- try(get_vectype(in_object), silent = TRUE)
  }

  if (class(obj_type) == "try-error") {
    stop_message <- paste0(
      "\"",deparse(substitute(in_object)),
      "\" is not a recognised spatial file.")
    if (abort==TRUE) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }

  return(obj_type)

}
