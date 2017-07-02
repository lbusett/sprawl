#' @title readshape
#' @description function for easily opening a ESRI shapefile (or other OGR compatible vector file)
#' by simply specifying its filename
#'
#' @param shp_file `character` Filename of ESRI shapefile to be opened
#' @param as_sp   `logical` If TRUE, the opened object is automatically converted to `sp` format.
#' Otherwise, an `sf` object is returned. Default: FALSE
#' @param ... other arguments to be passed to[sf::st_read]
#'
#' @return Spatial* object (SpatialPolygons, SpatialPoints, eccetera)
#' @export
#' @importFrom sf read_sf
#' @examples \dontrun{
#' # open a shapefile as a `sf` object
#'  shp_file = system.file("extdata","clip_shape.shp", package = "sprawl")
#'  readshape(shp_file)
#'
#' # open a shapefile as a `sp` object
#'  shp_file = system.file("extdata","clip_shape.shp", package = "sprawl")
#'  readshape(shp_file, as_sp = TRUE)
#'}
readshape = function(shp_file, as_sp = FALSE, ...){
  if (!file.exists(shp_file)) {
    stop("readshape --> Input file doesn't exist on your system. Aborting. ")
  }
  chk <- check_spatype(shp_file)
  if (chk == "vectfile") {
    shp <- sf::read_sf(shp_file, ...)
    if (as_sp) {
      shp <- as(shp, "Spatial")
    }
    return(shp)
  } else {
    stop("`shp_file` deosn't appear to correspond to a valid vector file ! Aborting !")
  }
}
