#' @title read a vector spatial file to R
#' @description function for easily opening a ESRI shapefile (or other OGR
#'   valid vector file by simply specifying its filename
#'
#' @param in_file `character` Filename of ESRI shapefile to be opened
#' @param as_sp   `logical` If TRUE, the opened object is automatically converted
#'   to `sp` format. Otherwise, an `sf` object is returned. Default: FALSE
#' @param ... other arguments to be passed to[sf::st_read]
#'
#' @return `sf` or `sp` object (depending on `as_sp` setting)
#' @details simple wrapper around `sf::read_sf`, with some checks on inputs and
#'   possibility of automatic re-casting to `*sp` objects
#' @export
#' @importFrom sf read_sf
#' @examples \dontrun{
#' library(sprawl.data)
#' # open a shapefile as a `sf` object
#'  in_file = system.file("extdata/shapes","lc_polys.shp",
#'                         package = "sprawl.data")
#'  read_vect(in_file)
#'
#' # open a shapefile as a `sp` object
#'  in_file = system.file("extdata/shapes","lc_polys.shp",
#'                         package = "sprawl.data")
#'  read_vect(in_file, as_sp = TRUE)
#'}
#'@seealso
#'  \code{\link[sf]{read_sf}}
#' @rdname read_vect
#' @export
#' @importFrom sf read_sf
#'
read_vect <- function(in_file, as_sp = FALSE, ...){

  checkmate::assertFileExists(in_file)
  chk <- get_vectype(in_file)
  shp <- sf::read_sf(in_file, ...)
  if (as_sp) {
    shp <- as(shp, "Spatial")
  }
  return(shp)
}
