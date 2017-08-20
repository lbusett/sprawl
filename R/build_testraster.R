#' @title build_testraster
#' @description helper function used to quickly create a `rasterStack` object with specified
#' dimensions (useful for testing purposes).
#' @param nrows `numeric` number of rows, Default: 100
#' @param ncols `numeric` number of columns, Default: 100
#' @param nbands `numeric` number of bands, Default: 1
#' @param with_nodata PARAM_DESCRIPTION, Default: TRUE
#' @param to_file `logical` if TRUE, the rasterStack is saved to a TIFF file, Default: FALSE
#' @param out_file `character` filename where the test raster should be saved (ignored if
#' `to_file` = NULL). If `out_file` is NULL, the file is saved in `R` temporary folder, otherwise the
#' filename specified in `out_file` is used. In both cases, the name of the created TIFF file is
#' returned instead than the `rasterStack` itself, Default: NULL.
#' @param ... Any other arguments to be passed to `raster::raster` while creating the output raster.
#' @return Either the "test" raster in `rasterStack` format, or the filename where it was saved (if
#' `tofile` = TRUE)
#' @note - Values of the test raster are integers between 1 and 250.
#' - If `out_file` is selected, the file is saved as a 8-bit unsigned integer tiff to save space.
#' - By default, the raster is created using a WGS84 lat/lon reference system, with extent
#' corresponding to the entire globe (bbox = -180, 180; -90, 90). To specify a different projection
#' and extent, specify them by passing the `crs` and `ext` arguments to `raster (see examples).
#' @examples
#' \dontrun{
#'  # create a rasterstack of 100*100 cells and 10 bands and put it in test_rast
#'  test_rast <- build_testraster(100,100,10)
#'  test_rast
#'
#'  # create a rasterstack of 100*100 cells and 20 bands and save it to a temporary TIFF file
#'  test_rast <- build_testraster(100,100,20, to_file = TRUE)
#'  test_rast
#'
#'  # create a rasterstack of 100*100 cells and 10 in epsg:3857 projection on a given extent
#'  build_testraster(100,100,10, crs = "+init=epsg:3857",
#'                               ext = raster::extent(c(0, 200000, 0, 200000)))
#'  test_rast
#' }
#' @rdname build_testraster
#' @export
#' @importFrom raster raster stack writeRaster
#' @importFrom stats runif

build_testraster <- function(nrows       = 100,
                             ncols       = 100,
                             nbands      = 1,
                             with_nodata = TRUE,
                             to_file     = FALSE,
                             out_file    = NULL,
                             ...) {

  # Build the raster ----
  for (b in 1:nbands) {
    if (b == 1) {
      # Build the band
      layer <- raster::raster(nrows = nrows,
                              ncols = ncols,
                              vals =  sample(1:50, (nrows * ncols),
                                             replace = TRUE), ...)
      # change to NA 1 out of each 200 cells at random cells
      if (with_nodata) {
        layer[stats::runif((nrows * ncols)/200, min = 1, max = nrows * ncols)] <- NA #nolint
      }
    } else {
      # if band != 1, create new layers as multiples of layer 1
      layer_new <- layer * sample(5, 1)
      # change to NA 1 out of each 500 cells random cells in each new layer (so
      # that NA is not always on the same cells and we have both cells with
      # "all NA" and cells with NA only in some layers)
      if (with_nodata) {
        layer_new[stats::runif((nrows * ncols)/200, min = 1, max = nrows * ncols)] <- NA #nolint
      }
    }
    if (b == 1) {
      stack <- layer
    } else {
      stack <- raster::stack(stack, layer_new, quick = TRUE)
    }
  }
  names(stack) <- paste0("band_", 1:nbands)

  # Save to file if necessary ----
  if (to_file) {
    if (is.null(out_file)) {
      out_file <- tempfile(fileext = ".tif")
    }
    tempfile <- raster::writeRaster(stack, filename = out_file,
                                    options = c("COMPRESS=LZW",
                                                datatype = "INT1U"))
    return(out_file)
  } else {
    return(stack)
  }
}
