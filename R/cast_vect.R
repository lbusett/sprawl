#' @title cast a vector object to a different `R` spatial class or to a shapefile
#' @description function to automatically re-cast a "vector" object to a given
#'  "class" specified by the `to` argument.
#' @param object either an `R` object of class `sf`, `sfc` or `Spatial`, or a
#'   character string corresponding to a filename (with full path)
#' @param to `character` indicating to which type of object the input should be
#'   re-casted. It can be "sfobject" (re-cast to `sf`), "spobject" (recast to
#'   `Spatial`) or "vectfile" (re-cast to a shapefile)
#' @return returns the same object, casted to the "class" specified by `to` (or
#'   the exact same object in case `object` is already of "class" `to`)
#' @details If `object` is a valid `R` spatial object, it is automatically
#'   converted to an object of a different class if needed (e.g., from `sf` to
#'   `Spatial` and viceversa, or from `sf` to a vector file through
#'   `sprawl::write_shape`). If it is a character string, the function checks if
#'   it corresponds to a valid vector file and reads it to a `sf` or `Spatial`
#'   object through `sprawl::read_vect`
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname cast_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

cast_vect <- function(object, to){
  UseMethod("cast_vect")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#'@method cast_vect default
#'@rdname cast_vect

cast_vect.default <- function(object, to) {
  call <- as.list(match.call())
  stop("cast_vect --> ", call[[1]], " is not a valid vector object or file. ",
       "Aborting !")
}

#   ____________________________________________________________________________
#   Method for sf                                                           ####

#'@export
#'@method cast_vect sf
#'@rdname cast_vect

cast_vect.sf <- function(object, to) {
  call <- as.list(match.call())
  if (to == "sfobject") return(object)
  if (to == "spobject") return(as(object, "Spatial"))
  if (to == "vectfile") {
    temp_shape <- tempfile(fileext = ".shp")
    write_shape(object, temp_shape)
    return(temp_shape)
  }
  stop("cast_vect --> `", as.character(call[[3]]), "` is invalid for `to`. ",
       "It should be \"sfobject\", \"spobject\" or \"vectfile\"")
}

#   ____________________________________________________________________________
#   Method for sfc                                                          ####

#'@export
#'@method cast_vect sfc
#'@rdname cast_vect

cast_vect.sfc <- function(object, to) {
  call <- as.list(match.call())
  if (to == "sfobject") return(object)
  if (to == "spobject") return(as(object, "Spatial"))
  if (to == "vectfile") {
    temp_shape <- tempfile(fileext = ".shp")
    write_shape(object, temp_shape)
    return(temp_shape)
  }
  stop("cast_vect --> `", as.character(call[[3]]), "` is invalid for `to`. ",
       "It should be \"sfobject\", \"spobject\" or \"vectfile\"")
}

#   ____________________________________________________________________________
#   Method for Spatial                                                      ####

#'@export
#'@method cast_vect Spatial
#'@rdname cast_vect

cast_vect.Spatial <- function(object, to) {
  call <- as.list(match.call())
  if (to == "spobject") return(object)
  if (to == "sfobject") return(sf::st_as_sf(object))
  if (to == "vectfile") {
    sf::st_as_sf(object)
    temp_shape <- tempfile(fileext = ".shp")
    write_shape(object, temp_shape)
    return(temp_shape)
  }
  stop("cast_vect --> `", as.character(call[[3]]), "` is invalid for `to`. ",
       "It should be \"sfobject\", \"spobject\" or \"vectfile\"")
}

#   ____________________________________________________________________________
#   Method for character                                                    ####

#'@export
#'@method cast_vect character
#'@rdname cast_vect

cast_vect.character <- function(object, to) {
  call <- as.list(match.call())
  check_vec <- get_spatype(object, abort = FALSE)
  if (check_vec == "vectfile") {
    if (to == "vectfile") return(object)
    if (to == "sfobject") return(read_vect(object))
    if (to == "spobject") return(as(read_vect(object), "Spatial"))
    stop("cast_vect --> `", as.character(call[[3]]), "` is invalid for `to`. ",
         "It should be \"sfobject\", \"spobject\" or \"vectfile\"")
  } else {
    stop("cast_vect --> ",  as.character(call[[2]]), " is not a valid vector ",
         "filename. Aborting !")
  }
}
