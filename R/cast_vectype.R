#' #' @title cast a vector to `sf` `sp` or shapefile
#' #' @description accessory function automatically recast an object to a `sf`, `sp` object, or to
#' #'   a shapefile on disk
#' #' @param in_vect either a `R` object or a `character` string potentially pointing to a vector file
#' #' @to in_vect either a `R` object or a `character` string potentially pointing to a vector file
#' #' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#' #'
#' #' @export
#' #' @importFrom gdalUtils ogrinfo gdalinfo
#' #' @examples \dontrun{
#' #' library(sprawl.data)
#' #' # input is a raster file
#' #' check_spatype(system.file("extdata","testrast.tif", package = "sprawl.data"))
#' #'
#' #' # input is a shapefile
#' #' shp_file <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#' #' check_spatype(shp_file)
#' #'
#' #' # input is a `sp` object
#' #' obj <- read_shape(shp_file, as_sp = TRUE)
#' #' check_spatype(obj)
#' #'
#' #' # input is a `sf` object
#' #' obj <- read_shape(shp_file)
#' #' check_spatype(obj)
#' #' }
#' #'
#' cast_vectype <- function(in_vect, to) {
#'
#'   if (to %in% c("spobject", "sfobject", "vectfile")) {
#'     stop("check_vectype --> invalid value in `to`. Set it to 'spobject', 'sfobject' or
#' 'vectfile'")
#'   }
#'
#'   vec_type <- "none"
#'
#'
#'
#'   if (vec_type == "none") {
#'     stop("check_vectype --> ", in_vect, " is not a vector (`*sf`` or `*sp` object or recognized
#'              vector file. Aborting !")
#'   } else {
#'     if (is.null(cast_to)) {
#'       return(vec_type)
#'     } else {
#'       if (vec_type != cast_to) {
#'         if (vec_type == "vectfile") {
#'           in_vect <- read_shape(in_vect)
#'           if (cast_to == "spobject") {
#'             in_vect <- as(in_vect, "Spatial")
#'           }
#'         }
#'
#'
#'         if (cast_to == "sfobject") {
#'           if (vec_type == "sfobject") {
#'             in_vect <- sf::st_as_sf(in_vect)
#'           } else {
#'
#'           }
#'         }
#'         if (cast_to == "spobject")
#'           in_vect <- as()
#'       }
#'     }
#'
#'   }
#' }
#' }
