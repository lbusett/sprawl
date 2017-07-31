#' @title automatic recasting between Raster object and raster files on disk
#' @description function to automatically re-cast a "vector" object to a given "class" specified by
#'   the `to` argument.
#' @param object either an `R` object of class `sf`, `sfc` or `Spatial`, or a character string
#'   corresponding to a filename (with full path)
#' @param to `character` indicating to which type of object the input should be re-casted. It can
#'   be "sfobject" (re-cast to `sf`), "spobject" (recast to `Spatial`) or "vectfile" (re-cast to
#'   a shapefile)
#' @return returns the same object, casted to the "class" specified by `to` (or the exact same
#'   object in case `object` is already of "class" `to`)
#' @details If `object` is a valid `R` spatial object, it is automatically converted to an object of
#'   a different class if needed (e.g., from `sf` to `Spatial` and viceversa, or from `sf` to a
#'   vector file through `sprawl::write_shape`). If it is a character string, the function checks if
#'   it corresponds to a valid vector file and reads it to a `sf` or `Spatial` object through
#'   `sprawl::read_vect`
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname cast_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

cast_rast <- function (object, to){
  UseMethod("cast_rast")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#'@method cast_rast default
#'@rdname cast_rast

cast_rast.default <- function (object, to) {
  call <- as.list(match.call())
  stop("cast_rast --> ", call[[1]], " is not a valid `Raster` object or raster file. Aborting !")
}

#   ____________________________________________________________________________
#   Method for Raster                                                        ####

#'@export
#'@method cast_rast Raster
#'@rdname cast_rast

cast_rast.Raster <- function(object, to) {
  call <- as.list(match.call())
  if (to == "rastobj") return(object)
  if (to == "rastfile") {
    if (object[[1]]@file@name == "") {
      temprastfile <- tempfile(fileext = ".tif")
      raster::writeRaster(object,
                          filename  = temprastfile,
                          options   = c("COMPRESS=DEFLATE"),
                          overwrite = TRUE)
      return(temprastfile)
    } else {
      object <- object[[1]]@file@name
      return(object)
    }
  }
  stop("cast_rast --> `", as.character(call[[3]]), "` is invalid for `to`. It should be `rastobj`,
       `rastfile`")
}

#   ____________________________________________________________________________
#   Method for character                                                    ####

#'@export
#'@method cast_rast character
#'@rdname cast_rast

cast_rast.character <- function(object, to) {

  call <- as.list(match.call())
  check_rast <- get_spatype(object)
  if (check_rast == "rastfile") {
    if (to == "rastfile") return(object)
    if (to == "rastobj") {
      if (file_ext(object) == "vrt") {
        return(raster::stack(object))
      } else {
        return(raster::brick(object))
      }
    }
    stop("cast_vect --> `", as.character(call[[3]]), "` is invalid for `to`. It should be `sfobject`,
       `spobject` or `vectfile`")
  }
  stop("cast_vect --> ",  as.character(call[[2]]), " is not a valid vector filename. Aborting !")
}
