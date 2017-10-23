#' @title add `info` attribute to a raster object
#' @description add an `info` attribute containing info retrieved by
#'   `sprawl::get_rastinfo` to a `Raster` object
#' @param object object of class `Raster`
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'
#'  setinfo_obj <- raster::brick(system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'                                           package = "sprawl.data"))
#'  setinfo_obj <- setinfo_rast(setinfo_obj)
#'  setinfo_obj@info
#'
#' @rdname setinfo_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

setinfo_rast <- function(object) {
  call <- match.call()
  if (inherits(object, "Raster")) {
    info <- get_rastinfo(object)
    attr(object, "info") <- info
    return(object)
  } else {
    stop(deparse(substitute(call)$object), " is not a `Raster` object. ",
                    "Aborting !")
  }
}
