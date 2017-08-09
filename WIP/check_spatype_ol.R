#' @title check_spatype
#' @description accessory function to check if an object passed to the function corresponds to
#' a `*Spatial` Object, a `sf` object, a R `raster` object, a file corresponding to a vector,
#' or a file corresponding to a raster
#' @param object either a `R` object or a `character` string pointing to a vector or raster layer
#' @return character vector equal to *spobject*, *sfobject*, *rastobject*, *vectfile* or *rastfile*
#'
#' @export
#' @importFrom gdalUtils ogrinfo gdalinfo
#' @examples \dontrun{
#' library(sprawl.data)
#' # input is a raster file
#' check_spatype(system.file("extdata","testrast.tif", package = "sprawl.data"))
#'
#' # input is a shapefile
#' shp_file <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
#' check_spatype(shp_file)
#'
#' # input is a `sp` object
#' obj <- read_shape(shp_file, as_sp = TRUE)
#' check_spatype(obj)
#'
#' # input is a `sf` object
#' obj <- read_shape(shp_file)
#' check_spatype(obj)
#' }
#'
check_spatype_old <- function(object) {

  sp_type <- "none"

  if (inherits(object,"Spatial")) {sp_type <- "spobject"}

  if (inherits(object,"sf") | inherits(object,"sfc"))      {sp_type <- "sfobject"}

  if (inherits(object,"Raster"))  {sp_type <- "rastobject"}

  if (sp_type == "none") {

    if (class(object) == "character") {

      if (file.exists(object)){

        vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))

        if (is.null(attr(vecttry, "status"))) {
          sp_type <- "vectfile"

        } else {

          rastry <- suppressWarnings(try(gdalUtils::gdalinfo(object), silent = TRUE))

          if (is.null(attr(rastry, "status"))) {

            sp_type <- "rastfile"

          } else {
            sp_type <- "none"
          }
        }
      }

    } else {
      sp_type <- "none"
    }
  }
  return(sp_type)
}
