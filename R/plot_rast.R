#' @title quick plot for a raster based on level_plot
#' @description FUNCTION_DESCRIPTION
#' @param in_rast Input raster object or file
#' @param band PARAM_DESCRIPTION, Default: 1
#' @param in_poly optional input polygon vector object or file to be overlayed on
#'   the plot, Default: NULL
#' @param in_points optional input points vector object or file to be overlayed
#'  on the plot, Default: NULL
#' @param background not used, Default: FALSE
#' @param limits `numeric array [2]` optional limits governing the range of
#'  values to be plotted (e.g., c(0.2,0.4)), Default: NULL
#' @param tails `numeric array [2]` percentiles used to "cut" the values to be
#'  plotted to allow a "good" representation. Values outside the specified
#'  percentiles will be plotted as NoData, or using the colors specified in
#'  `col_outlow` and `col_outhigh`, Default: c(0.02, 0.98) (meaning cutting the
#'  values at the 2nd and 98th percentile)
#' @param palette Palette to be used for colors (see [`RColorBrewer::brewer.pal`]),
#'  Default: 'RdYlGn'
#' @param legend_type "standard" or "custom" (see examples), Default: 'standard'
#' @param col_outlow Color used to plot the values below the lower limit/tail.
#'   Can be a `character` corresponding to a valid "R" color or HEX representation,
#'   Default: 'gray10'
#' @param col_outhigh Color used to plot the values below the lower limit/tail.
#'  Can be a `character` corresponding to a valid "R" color or HEX representation,
#'   Default: 'gray90'
#' @param maxpixels `numeric` Maximum number of pixels to be plotted. **Increase
#'  this to have better rendering at the cost of speed!**, Default: 5e+05
#' @param title `character` Title for the plot, Default: 'Raster Plot'
#' @param plot_now `logic` If TRUE, the plot is immediately printed to screen.
#'  Otherwise, an object allowing later plotting/modifications is returned,
#'   Default: TRUE
#' @param ... Any other arguments (?)
#' @return OUTPUT_DESCRIPTION

#' @details DETAILS
#' @examples
#'
#'  library(sprawl)
#'
#'  in_rast <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'   package = "sprawl.data")
#'  in_vect <- create_fishnet(in_rast, pix_for_cell = 150)
#'  # plot only the raster
#'  plot_rast(in_rast)
#'
#'  # plot only the raster with custom legend
#'  plot_rast(in_rast, legend = "custom")
#'
#'  # add a polygon and change the legend, palette and maxpixels
#'  plot_rast(in_rast,
#'            in_poly   = in_vect,
#'            tails     = c(0.1, 0.99),
#'            legend    = "custom",
#'            palette   = "RdYlBu" ,
#'            title     = "RapidEye - GNDVI",
#'            maxpixels = 10e5)
#'
#'  # Plot more than one band
#'  in_rast <- raster::stack(system.file(
#'                           "extdata/OLI_test", "oli_multi_1000.tif",
#'                           package = "sprawl.data"))[[2:5]]
#'  plot_rast(in_rast, legend = "custom", maxpixels = 1e5)
#'
#' @seealso
#'  \code{\link[rasterVis]{levelplot}}
#'  \code{\link[RColorBrewer]{brewer.pal}}
#'
#' @rdname plot_rast
#' @export
#' @importFrom latticeExtra layer
#' @importFrom raster stack quantile
#' @importFrom rasterVis levelplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom sp sp.polygons sp.points
plot_rast <- function(in_rast,
                      band        = 1,
                      in_poly     = NULL,
                      in_points   = NULL,
                      background  = FALSE,
                      limits      = NULL,
                      tails       = c(0,1),
                      palette     = "RdYlGn",
                      legend_type = "standard",
                      col_outlow  = "gray10",
                      col_outhigh = "gray90",
                      maxpixels   = 5e5,
                      title       = "Raster Plot",
                      plot_now    = TRUE,
                      ...) {

  # To avoid NOTES on check
  x <- NULL

  #TODO Reshape to use theme template.
  if (get_rastype(in_rast) == "rastfile") {
    in_rastplot <- read_rast(in_rast)
  } else {
    in_rastplot <- in_rast
  }


  if (!is.null(in_poly)) {
    in_poly    <- cast_vect(in_poly, "sfobject")
    proj4_rast <- get_proj4string(in_rast)
    proj4_vect <- get_proj4string(in_poly)
    if (proj4_vect != proj4_rast) {
      in_poly <- sf::st_transform(in_poly, proj4_rast)
    }
  }

  #   __________________________________________________________________________
  #   If limits not passed, compute limits for the plot on the basis of     ####
  #   the values of "cut_tails" and of the distribution of values in in_rast
  if (raster::nlayers(in_rastplot) == 1) {
    if (is.null(limits)) {
      limits <- raster::quantile(in_rastplot, probs = c(0, tails, 1),
                                 ncells = maxpixels)
    } else {
      limits <- c(limits[1], limits[1], limits[2], limits[2])
    }
  } else {
    if (is.null(limits)) {
      quantiles <- raster::quantile(in_rastplot, probs = c(0, tails, 1),
                                    ncells = maxpixels)
      limits <- c(min(quantiles[,1]), min(quantiles[,2]),
                  max(quantiles[,3]), max(quantiles[,4]))
    } else {
      limits <- c(limits[1], limits[1], limits[2], limits[2])
    }
  }

  if (limits[1] == limits[2]) limits[2] <- limits[1] + 0.000001
  if (limits[3] == limits[4]) limits[3] <- limits[4] - 0.000001

  #   __________________________________________________________________________
  #   set up the color table                                                ####

  if (legend_type == "standard") {
    my.col <- c(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, palette))(100) #nolint
    )
    at <- c(seq(limits[2], limits[3], (limits[3] - limits[2])/100))
  }

  if (legend_type == "custom") {
    my.col <- c(col_outlow,   # color for values below lower limit
                grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, palette))(98), #nolint
                col_outhigh)   # color for values abvove lower limit
    at <- c(limits[1], seq(limits[2],
                           limits[3],
                           (limits[3] - limits[2])/98), limits[4])
  }

  rastplot <- rasterVis::levelplot(in_rastplot,
                                   margin      = FALSE,
                                   col.regions = my.col,
                                   at          = at,
                                   maxpixels   = maxpixels,
                                   main        = title,
                                   xlab        = NULL,
                                   ylab        = NULL,
                                   scales      = list(draw = FALSE))

  case_identifier <- 0
  if (!is.null(in_poly)) {

    polys <- as(in_poly, "Spatial")
    polyplot  <- latticeExtra::layer(sp::sp.polygons(x), data = list(x = polys))
    case_identifier <- case_identifier + 10
  } else {
    polyplot <- NULL
  }

  if (!is.null(in_points)) {
    polys <- as(in_points, "Spatial")
    pointplot <- latticeExtra::layer(sp::sp.points(x), data = list(x = polys))
    case_identifier <- case_identifier + 100
  } else {
    pointplot <- NULL
  }

  if (plot_now) {
    if (case_identifier == 0)   print(rastplot)
    if (case_identifier == 10)  print(rastplot + polyplot)
    if (case_identifier == 110) print(rastplot + polyplot + pointplot)
  }
}
