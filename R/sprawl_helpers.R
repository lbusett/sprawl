#' @title build_testraster
#' @description FUNCTION_DESCRIPTION
#' @param nrows `numeric` number of rows, Default: 100
#' @param ncols `numeric` number of columns, Default: 100
#' @param nbands `numeric` number of bands, Default: 1
#' @param with_nodata PARAM_DESCRIPTION, Default: TRUE
#' @param tofile `logical` if TRUE, the rasterStack is saved to a temporary file
#' and the name of the temporary filename is returned instead than the stack itself,
#' Default: FALSE
#' @return either the "test" `rasterStack`, or a filename corresponding to it (if
#' `tofile` = TRUE)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  r <- build_testraster(100,100,10, tofile = F)
#'  }
#' }
#' @seealso
#'
#' @rdname build_testraster
#' @export
#' @importFrom raster raster stack writeRaster
#' @importFrom magrittr "%>%"

build_testraster <- function(nrows       = 100,
                             ncols       = 100,
                             nbands      = 1,
                             with_nodata = TRUE,
                             tofile      = FALSE) {
  for (b in 1:nbands) {
    if (b == 1) {
      layer <- raster::raster(nrows = nrows,
                              ncols = ncols,
                              vals =  stats::rnorm(nrows*ncols, mean = 2, sd  = 1))
      if (with_nodata) {
        layer[stats::runif((nrows * ncols)/200, min = 1, max = nrows * ncols)] = NA
      }
    } else {
      layer_new <- layer * sample(10, 1)
      if (with_nodata) {
        layer_new[stats::runif((nrows * ncols)/200, min = 1, max = nrows * ncols)] = NA
      }
    }

    if (b == 1) {
      stack <- layer
    } else {
      stack <- raster::stack(stack, layer_new, quick = TRUE)
    }
  }
  names(stack) <- paste0("band_", 1:nbands)

  if (tofile) {
    tempfilename <- tempfile(fileext = ".tif")
    tempfile <- raster::writeRaster(stack, filename = tempfilename)
    return(tempfilename)
  } else {
    return(stack)
  }

}

#' @title build_testshape
#' @description FUNCTION_DESCRIPTION
#' @param maxpolys PARAM_DESCRIPTION
#' @param allow_overlaps PARAM_DESCRIPTION, Default: FALSE
#' @param tofile PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  r <- build_raster(100, 100, 1)
#'  p <- build_testshape(100)
#'  plot(r)
#'  plot(p[1], col = p$id, add = T)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname build_testshape
#' @export
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf st_buffer st_set_crs st_intersection st_difference st_union
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
#'
build_testshape <- function(maxpolys,
                            allow_overlaps = FALSE,
                            tofile         = FALSE) {
  # set.seed(100)
  rmax     <- 10
  x_limits <- c(-180,180)
  y_limits <- c(-90,90)
  n_separate <- 0
  xy <- data.frame(
    id = paste0("id_", 1:maxpolys),
    x = stats::runif(maxpolys, min(x_limits), max(x_limits)),
    y = stats::runif(maxpolys, min(y_limits), max(y_limits))) %>%
    tibble::as_tibble()
  polys <- sf::st_as_sf(xy, coords = c(2,3)) %>%
    sf::st_buffer(stats::runif(maxpolys, min = 2, max = rmax)) %>%
    sf::st_set_crs(4326)

  if (!allow_overlaps) {
    int <- sf::st_intersection(polys, polys) %>%
      dplyr::filter(id != id.1) %>%
      sf::st_as_sf()
    if (length(int$id) > 0) polys <- sf::st_difference(polys, sf::st_union(int))
  }
  
  if (tofile) {
    tempfilename <- tempfile(fileext = ".shp")
    tempfile <- writeshape(polys, out_file = tempfilename)
    return(tempfilename)
  } else {
    return(polys)
  }
}
