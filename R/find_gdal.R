#' @title find main folder of gdal installation
#' @description Helper function to identify the folder of the GDAL installation,
#'   and its version
#' @return `character` full path to the gdal installation folder. The returned
#'   object contains a `version` attribute that can be used to retrieve the
#'   GDAL version. The functoin first checks if the path was already retrieved
#'   and set in the "sprawl_gdalpath" global variable. If not, it tries to locate
#'   the path using (in sequence):
#'     1. Sys.which()
#'     2. gdalUtils::gdal_setInstallation(ignore_fullscan = TRUE)
#'     3. gdalUtils::gdal_setInstallation(ignore_fullscan = FALSE)
#'
#'  If all fails, the function aborts. If a valid installation is found, then
#'  the global variable "prawl_gdalpath" is set (this allows not to have
#'  to continuosly look for the gdal folder on later executions)
#' @examples
#'  gdalpath <- find_gdal()
#'  gdalpath
#'
#' @rdname find_gdal
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gdalUtils gdal_setInstallation
#' @importFrom magrittr "%>%"
find_gdal <- function() {
  . <- NULL
  gdalpath <- options()$sprawl_gdalpath
  if (is.null(gdalpath)) {
    gdalpath <- dirname(Sys.which("gdalinfo"))
    if (!(gdalpath == "")) {
      gdalversion <- system2("gdalinfo", args = "--version", stdout = TRUE) %>%
        strsplit(., ",") %>% `[[`(1) %>% `[[`(1) %>%
        strsplit(., " ") %>% `[[`(1) %>% `[[`(2)
      attr(gdalpath, "version") <- gdalversion
      options("sprawl_gdalpath" = gdalpath)
    } else {
      gdalpath <- getOption("gdalUtils_gdalPath")[[1]]$path
      if (is.null(gdalpath)) {
        gdalUtils::gdal_setInstallation()
        gdalpath <- getOption("gdalUtils_gdalPath")[[1]]$path
      }
      if (is.null(gdalpath)) {
        gdalUtils::gdal_setInstallation(ignore.full_scan = FALSE)
        gdalpath <- getOption("gdalUtils_gdalPath")[[1]]$path
      }
      if(!(is.null(gdalpath))) {
        gdalversion <- system2("gdalinfo", args = "--version", stdout = TRUE) %>%
          strsplit(., ",") %>% `[[`(1) %>% `[[`(1) %>%
          strsplit(., " ") %>% `[[`(1) %>% `[[`(2)
        options("sprawl_gdalpath" = gdalpath)
      } else {
        stop("sprawl --> Unable to locate the GDAL installation folder. Aborting!",
             "\nPlease ensure that GDAL is installed and on your PATH!")
      }
    }
  }
  options()$sprawl_gdalpath
}
