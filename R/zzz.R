#' @title helper to set classes on package loading
#' @description helper to set classes on package loading
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname setClasses
#' @export
#' @importFrom methods setClass representation
#' @exportClass sprawlext
setClasses <- function() {

  #' @name sprawlext-class
  #' @rdname sprawlext-class
  #' @description An S4 class to represent the extent of a spatial object, associated with its proj4
  #' string.
  #'
  #' @slot extent `numeric (4)` extent of the object (xmin, ymin, xmanx, ymax)
  #' @slot projstring character` proj4string of the object
  #' @exportClass sprawlext
  #' @export
  #' @importFrom methods setClass representation

  methods::setClass("sprawlext",
                    methods::representation(extent = "numeric",
                                            projstring = "character"))
}

setClasses()
