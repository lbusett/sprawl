#' @title automatic recasting between Raster object and raster files on disk
#' @description function to automatically re-cast between Raster object and raster files on disk
#' @param object either an `R` object of class `sf`, `sfc` or `Spatial`, or a character string
#'   corresponding to a filename (with full path)
#' @param to `character` indicating to which type of object the input should be re-casted. It can
#'   be "rastobject" (re-cast to `Raster`) or "rastfile" (recast to raster file).In the
#'   second case, recasting is achieved by identifying the raster file associated with the
#'   Raster object and returning its path. If no file is associated (i.e., the object is
#'   in-memory), the Raster object is written to disk on a temporary file, which path is returned
#' @return returns the same object, casted to the "class" specified by `to` (or the exact same
#'   object in case `object` is already of "class" `to`)
#' @details If `object` is a valid `R` spatial object, it is automatically converted to an object of
#'   a different class if needed (e.g., from `sf` to `Spatial` and viceversa, or from `sf` to a
#'   vector file through `sprawl::write_shape`). If it is a character string, the function checks if
#'   it corresponds to a valid raster file and reads it to a `Raster` object through
#'   `rasterr::stack()`
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname cast_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

cast_rast <- function(object,
                      to){
  UseMethod("cast_rast")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#'@method cast_rast default
#'@rdname cast_rast

cast_rast.default <- function(object, to) {
  call <- as.list(match.call())
  stop("cast_rast --> ",
       call[[1]], " is not a valid `Raster` object or raster file. Aborting !")
}

#   ____________________________________________________________________________
#   Method for Raster                                                       ####

#'@export
#'@method cast_rast Raster
#'@rdname cast_rast

cast_rast.Raster <- function(object, to) {

  checkmate::assert_choice(to, c("rastobject", "rastfile"))
  if (to == "rastobject") return(object)
  if (to == "rastfile") {
    info <- get_rastinfo(object, verbose = FALSE, stats = FALSE)
    if (any(info$fnames == "")) {
      temprastfile <- tempfile(fileext = ".tif")
      raster::writeRaster(object,
                          filename  = temprastfile,
                          options   = c("COMPRESS=DEFLATE"),
                          overwrite = TRUE)
      return(temprastfile)
    } else {
      return(info$fnames)
    }
  }
}

#   ____________________________________________________________________________
#   Method for character                                                    ####

#'@export
#'@method cast_rast character
#'@rdname cast_rast

cast_rast.character <- function(object, to) {

  checkmate::assert_choice(to, c("rastobject", "rastfile"))
  checkmate::assert_file_exists(object, "r")

  check_rast <- get_rastype(object)
  if (to == "rastfile") {
    return(object)
  } else {
    return(read_rast(object))

  }
}
