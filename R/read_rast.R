#' @title read a raster file from disk
#' @description reads a raster file to a `RasterLayer`, `RasterStack` or
#'   `RasterBrick`, automatically checking if the input file is valid and is
#'   a single- or multi-band object.
#' @param object `character` filename to be read
#' @param bands_to_read `numeric array` If the input is a multiband raster,
#'   band numbers to be read (e.g., bands_to_read = c(1,5,7) will read only the
#'   specified bands in a `RasterStack`)
#' @param verbose `logical` If FALSE, processing mesasges are suppressed,
#'  Default: TRUE
#' @return a `RasterLayer`, `RasterStack` or `RasterBrick` object
#' @details DETAILS
#' @examples
#' \dontrun{
#'  # read a singleband file
#'   in_file <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data")
#'   read_rast(in_file)
#'
#'   # read a multiband file
#'   in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                           package = "sprawl.data")
#'   read_rast(in_file)
#'
#'   # read a multiband file selecting specific bands
#'   read_rast(in_file, bands_to_read = c(1,10))
#'
#' }
#' @rdname read_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that is.readable
#' @importFrom raster raster brick stack
read_rast <- function(object,
                      bands_to_read = NULL,
                      verbose = TRUE) {

  #   __________________________________________________________________________
  #   check arguments                                                       ####

  assertthat::assert_that(
    assertthat::is.readable(object),
    msg = strwrap("read_rast --> `object` is not a valid filename, or is not
                  readable. Aborting!")
  )

  assertthat::assert_that(
    system2(file.path(find_gdal(), "gdalinfo"), args = object, stderr = NULL,
            stdout = NULL) != 1,
    msg = strwrap("read_rast --> `object` is not a valid raster file.
                  Aborting!")
  )

  rastinfo <- get_rastinfo(object)
  if (rastinfo$nbands == 1) {
    return(raster::raster(object))
  } else {
    if (length(unique(rastinfo$fnames) == 1) & is.null(bands_to_read)) {
      return(raster::brick(object))
    } else {
      return(raster::stack(object, bands = bands_to_read))
    }
  }
}
