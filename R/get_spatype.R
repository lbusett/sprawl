#' @title check the "spatial type" of an object or file
#' @description accessory function to check if an object passed to the function
#'  corresponds to a `*Spatial` Object, a `sf` object, a R `raster` object, a
#'  file corresponding to a vector, or a file corresponding to a raster.
#'  NOTE: to check only for vector or raster types, the associated functions
#'  `get_vectype` and `get_rastype` can be used, with the same syntax.
#' @param in_object either a `R` object or a `character` string pointing to a
#'  vector or raster layer
#' @param abort `logical` if TRUE the function aborts if `object` is not
#' recognized as an R spatial file or valid vector or raster file; if FALSE,
#' a warning is shown and NA is returned.
#' @return character (\"*spobject*\" | \"*sfobject*\" | \"*rastobject* | \"
#' *vectfile*\" | *rastfile* \" | *sprawlext*), or "NA" if the input does not
#' belong to any spatial category and abort == FALSE
#' @name get_spatype
#' @rdname get_spatype
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @examples
#'
#' library(sprawl.data)
#' # input is a raster file
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                         package = "sprawl.data")
#' get_spatype(in_rast)
#'
#' # input is a raster file
#' obj <- read_rast(in_rast)
#' get_spatype(in_rast)
#'
#' # input is a shapefile
#' in_vect <- system.file("extdata/shapes","lc_polys.shp",
#'            package = "sprawl.data")
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

get_spatype <- function(in_object,
                        abort = TRUE) {
  obj_type <- suppressWarnings(
    get_rastype(in_object, abort = FALSE)
  )

  if (is.na(obj_type)) {
    obj_type <-  suppressWarnings(
      get_vectype(in_object, abort = FALSE)
    )
  }
  if (is.na(obj_type)) {
    stop_message <- paste0(
      "\"", deparse(substitute(in_object)),
      "\" is not a recognised vector/raster object or filename")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }

  return(obj_type)
}
