# check if all elements of a vector are valid "R" colors
#

#' @title check if an array represents `R` colors
#' @description check if all elements of a vector are valid "R" colors
#' @param x `character array` containing color names
#' @return `logical array` with value TRUE for entries correasponding to
#'   colors, FALSE for invalid entries
#' @examples
#' areColors(c("red", "green", "#C74C4C"))
#'
#' areColors(c("red", "fdfd"))
#'
#' @rdname areColors
#' @export
#' @author Josh O'Brien - https://stackoverflow.com/a/13290832/6871135
#' importFrom grDevices col2rgb
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}
