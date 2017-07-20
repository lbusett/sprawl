#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param band PARAM_DESCRIPTION, Default: 1
#' @param in_poly PARAM_DESCRIPTION, Default: NULL
#' @param in_points PARAM_DESCRIPTION, Default: NULL
#' @param background PARAM_DESCRIPTION, Default: FALSE
#' @param limits PARAM_DESCRIPTION, Default: NULL
#' @param tails PARAM_DESCRIPTION, Default: c(0.01, 0.99)
#' @param palette PARAM_DESCRIPTION, Default: 'RdYlGn'
#' @param legend_type PARAM_DESCRIPTION, Default: 'standard'
#' @param col_outlow PARAM_DESCRIPTION, Default: 'gray10'
#' @param col_outhigh PARAM_DESCRIPTION, Default: 'gray90'
#' @param maxpixels PARAM_DESCRIPTION, Default: 1e+05
#' @param title PARAM_DESCRIPTION, Default: 'Raster Plot'
#' @param plot_now PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(sprawl)
#'
#'  in_rast <- system.file("extdata", "gNDVI.tif", package = "sprawl.data")
#'  in_poly <- create_fishnet(in_rast, pix_for_cell = 150)

#'  #'  # plot only the raster
#'  plot_rast(in_rast, in_poly = in_poly)
#'
#'  # add a polygon and change the legend

#'  plot_rast(in_rast,
#'            in_poly   = in_vect,
#'            tails     = c(0.1, 0.99),
#'            legend    = "standard",
#'            palette   = "RdYlBu" ,
#'            maxpixels = 1e5)
#'  }
#' }
#' @seealso
#'  \code{\link[latticeExtra]{layer}}

#'  \code{\link[raster]{stack}},\code{\link[raster]{quantile}}

#'  \code{\link[rasterVis]{levelplot}}

#'  \code{\link[RColorBrewer]{brewer.pal}}
#' @rdname plot_rast
#' @export
#' @importFrom latticeExtra layer
#' @importFrom raster stack quantile
#' @importFrom rasterVis levelplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp sp.polygons sp.points
plot_rast <- function(in_rast,
                      band        = 1,
                      in_poly     = NULL,
                      in_points   = NULL,
                      background  = FALSE,
                      limits      = NULL,
                      tails       = c(0.02, 0.98),
                      palette     = "RdYlGn",
                      legend_type = "standard",
                      col_outlow  = "gray10",
                      col_outhigh = "gray90",
                      maxpixels   = 1E5,
                      title       = "Raster Plot",
                      plot_now    = TRUE,
                      ...) {

  #TODO Allow inputting a raster FILE
  if (check_spatype(in_rast) == "rastfile") {
    in_rastplot <- raster::stack(in_rast)
  } else {
    in_rastplot <- in_rast
  }

  #   ____________________________________________________________________________
  #   If limits not passed, compute limits for the plot on the basis of       ####
  #   the values of "cut_tails" and of the distribution of values in in_rast

  if (is.null(limits)) {
    limits <- raster::quantile(in_rastplot, probs = c(0, tails, 1), ncells = 5000000000)
  } else {
    limits <- c(limits[1], limits[1], limits[2], limits[2])
  }

  #   ____________________________________________________________________________
  #   set up the color table                                                  ####

  if (legend_type == "standard") {
    my.col <- c(colorRampPalette(RColorBrewer::brewer.pal(11, palette))(100))
    at = c(seq(limits[2], limits[3], (limits[3] - limits[2])/100))
  }

  if (legend_type == "custom") {
    my.col <- c(col_outlow,   # color for values below lower limit
                colorRampPalette(RColorBrewer::brewer.pal(11, palette))(98),
                col_outhigh)   # color for values abvove lower limit
    at = c(limits[1], seq(limits[2], limits[3], (limits[3] - limits[2])/98), limits[4])
  }

  rastplot <- rasterVis::levelplot(in_rastplot,
                                   margin      = FALSE,
                                   col.regions = my.col,
                                   at          = at,
                                   maxpixels   = maxpixels,
                                   main        = title)

  case_identifier <- 0
  if (!is.null(in_poly)) {
    # invisible(in_poly)
    polyplot  <- latticeExtra::layer(sp::sp.polygons(as(in_poly, "Spatial")))
    case_identifier <- case_identifier + 10
  } else {
    polyplot <- NULL
  }

  if (!is.null(in_points)) {
    pointplot <- latticeExtra::layer(sp::sp.points(as(in_points, "Spatial")))
    case_identifier <- case_identifier + 100
  } else {
    pointplot <- NULL
  }
 # browser()
  if (plot_now) {
    if (case_identifier == 0)   print(rastplot)
    if (case_identifier == 10)  print(rastplot + polyplot)
    if (case_identifier == 110) print(rastplot + polyplot + pointplot)
  }
}
