#' @title check the "spatial type" of an object or file
#' @rdname get_spatype
#' @export
#' @importFrom gdalUtils ogrinfo
#' @importFrom checkmate assert_file_exists

get_vectype  <- function(in_object, abort = TRUE) {
  UseMethod("get_vectype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_vectype default
#' @export
get_vectype.default <- function(in_object, abort = TRUE) {
  stop_message <- paste0("\"", deparse(substitute(in_object)),
                         "\" is not a recognised vector object or filename.")
  if (abort) {
    stop(stop_message)
  } else {
    warning(stop_message)
    return(NA)
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_vectype character
#' @importFrom gdalUtils ogrinfo
#' @importFrom checkmate assert_file_exists
#' @export
get_vectype.character <- function(in_object, abort = TRUE) {
  checkmate::assert_file_exists(in_object, access = "r")
  vecttry <- suppressWarnings(
    try(gdalUtils::ogrinfo(in_object, al = TRUE,
                           so = TRUE,
                           verbose = FALSE,
                           q = TRUE),
        silent = TRUE)
  )
  if (is.null(attr(vecttry, "status"))) {
    return("vectfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_object)),
                           "\" is not a recognised vector filename.")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @method get_vectype sf
#' @export
get_vectype.sf <- function(in_object, abort = TRUE) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                       ####

#' @method get_vectype sfc
#' @export
get_vectype.sfc  <- function(in_object, abort = TRUE) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @method get_vectype Spatial
#' @export
get_vectype.Spatial <- function(in_object, abort = TRUE) {
  "spobject"
}

#   ____________________________________________________________________________
#   Method for "sprawlext"                                                 ####

#' @method get_vectype Spatial
#' @export
get_vectype.sprawlext <- function(in_object, abort = TRUE) {
  "sprawlext"
}
