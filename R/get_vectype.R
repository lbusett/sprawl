#' @title check the "spatial type" of an object or file
#' @rdname get_spatype
#' @export
#' @importFrom gdalUtils ogrinfo

get_vectype  <- function(in_object) {
  UseMethod("get_vectype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_vectype default
#' @export
get_vectype.default <- function(in_object) {
    stop("get_vectype --> ", deparse(substitute(in_object)),
         " is not a R object or filename. ",
         "Aborting!")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_vectype character
#' @export
get_vectype.character <- function(in_object) {
  checkmate::assert_file_exists(in_object, access = "r")
  vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(in_object, al = TRUE,
                                                     so = TRUE,
                                                     verbose = FALSE,
                                                     q = TRUE),
                                  silent = TRUE))
  if (is.null(attr(vecttry, "status"))) {
    return("vectfile")
  } else {
    stop("get_vectype --> ", deparse(substitute(in_object)),
         " is not a valid vector file. ",
         "Aborting!")
  }
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @method get_vectype sf
#' @export
get_vectype.sf <- function(in_object) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

#' @method get_vectype sfc
#' @export
get_vectype.sfc  <- function(in_object) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @method get_vectype Spatial
#' @export
get_vectype.Spatial <- function(in_object) {
  "spobject"
}

#   ____________________________________________________________________________
#   Method for "sprawlext"                                                    ####

#' @method get_vectype Spatial
#' @export
get_vectype.sprawlext <- function(in_object) {
  "sprawlext"
}

