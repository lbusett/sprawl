#' lb_theme_bw
#'
#' @param x_ang angle for x labels
#' @param y_ang angle for y labels
#' @param xsize_t size for x title
#' @param ysize_t size for y title
#' @param xsize size for x labels
#' @param ysize size for y labels
#' @param tsize size for title
#'
#' @return NULL
#'
#' @importFrom ggplot2 theme_bw
#' @export
#'

lb_theme_bw = function(x_ang = NULL, y_ang = NULL, xsize_t = NULL, ysize_t = NULL, xsize = NULL, ysize= NULL, tsize = NULL) {

  themeMod <- theme_bw() + theme(axis.title.x = element_text(margin = margin(8, 0, 0, 0)) ,axis.title.y = element_text(margin = margin(0, 8, 0, 0)) )

  if(length(x_ang == 1)) {themeMod = themeMod + theme(axis.text.x = element_text(angle = x_ang))}
  if(length(y_ang == 1)) {themeMod = themeMod + theme(axis.text.y = element_text(angle = x_ang))}
  if(length(xsize_t == 1)) {themeMod = themeMod + theme(axis.title.x = element_text(size = xsize_t))}
  if(length(ysize_t == 1)) {themeMod = themeMod + theme(axis.title.y = element_text(size = ysize_t))}
  if(length(xsize == 1)) {themeMod = themeMod + theme(axis.text.x = element_text(size = xsize_t))}
  if(length(ysize == 1)) {themeMod = themeMod + theme(axis.text.y = element_text(size = ysize_t))}
  if(length(tsize == 1)) {themeMod = themeMod + theme(title = element_text(size = tsize))}
  Sys.setlocale("LC_ALL","English")
  themeMod

}
