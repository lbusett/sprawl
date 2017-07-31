#' @title find gdal main folder
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname find_gdal
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gdalUtils gdal_setInstallation
find_gdal <- function() {
  if (is.null(options()$sprawl_gdalpath)) {
   gdalUtils::gdal_setInstallation()
   options("sprawl_gdalpath" = getOption("gdalUtils_gdalPath")[[1]]$path)
  }
  options()$sprawl_gdalpath
}
