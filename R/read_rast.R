#' @title read a raster file from disk
#' @description reads a raster file to a `RasterLayer`, `RasterStack` or
#'   `RasterBrick`, automatically checking if the input file is valid and is
#'   a single- or multi-band object.
#' @param in_file `character` filename to be read
#' @param bands `numeric array` If the input is a multiband raster,
#'   band numbers to be read (e.g., bands = c(1,5,7) will read only the
#'   specified bands in a `RasterStack`)
#' @param verbose `logical` If FALSE, processing messages are suppressed,
#'  Default: TRUE
#' @return a `RasterLayer`, `RasterStack` or `RasterBrick` object
#' @details DETAILS
#' @examples
#'
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
#'   read_rast(in_file, bands = c(1,10))
#'
#'
#' @rdname read_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that is.readable
#' @importFrom checkmate assertFileExists test_set_equal test_class
#' @importFrom raster raster brick stack
read_rast    <- function(in_file,
                         bands = NULL,
                         verbose       = TRUE) {
  #   __________________________________________________________________________
  #   check arguments                                                       ####
  #

  call <- match.call()
  checkmate::assertFileExists(in_file, "r")

  info <- try(rgdal::GDALinfo(in_file, silent = TRUE), silent = TRUE)
  if (!checkmate::test_class(info, "GDALobj")) {
    stop("read_rast --> `", eval(call[[2]]), "` is not a valid raster file. ",
         "Aborting!")
  }

  if (info[["bands"]] == 1) {
    # On single band raster, return a RasterLayer
    return(raster::raster(in_file))
  } else {
      # On multi band raster non vrt, return a RasterBrick, unless only
      # one band is requested
      if (is.null(bands)) {
        return(raster::brick(in_file))
      } else {
        if (length(bands) == 1) {
          return(raster::raster(in_file, band = bands))
        } else {
          return(raster::stack(in_file, bands = bands))
        }
      }
    }


  }
