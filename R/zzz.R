#' @title helper to set classes on package loading
#' @description helper to set classes on package loading
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname setClasses
#' @export
#' @importFrom methods setClass representation
#' @exportClass sprawlext
setClasses <- function() {

  system.file("R","sprawlext-class.R", package="sprawl")

}
setClasses()
