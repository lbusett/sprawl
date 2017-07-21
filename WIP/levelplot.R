plot_rast <- function(in_rast,
                      band        = 1,
                      in_poly     = NULL,
                      in_points   = NULL,
                      background  = FALSE,
                      limits      = NULL,
                      tails       = c(0.01, 0.99),
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
    in_rast <- raster::stack(in_rast)
  }

  #   ____________________________________________________________________________
  #   If limits not passed, compute limits for the plot on the basis of       ####
  #   the values of "cut_tails" and of the distribution of values in in_rast

  if (is.null(limits)) {
    limits <- raster::quantile(in_rast, probs = c(0, tails, 1), ncells = 5000000000)
  }

  #   ____________________________________________________________________________
  #   set up the color table                                                  ####

  if (legend_type == "standard") {
    my.col <- c(colorRampPalette(RColorBrewer::brewer.pal(11, palette))(100))
    at = c(seq(limits[2], limits[3], (limits[3] - limits[2])/50))
  }

  if (legend_type == "custom") {
    my.col <- c(col_outlow,   # color for values below lower limit
                colorRampPalette(RColorBrewer::brewer.pal(11, palette))(98),
                col_outhigh)   # color for values abvove lower limit
    at = c(limits[1], seq(limits[2], limits[3], (limits[3] - limits[2])/48), limits[4])
  }

  rastplot <- rasterVis::levelplot(in_rast,
                                   margin = FALSE,
                                   col.regions = my.col,
                                   at = at,
                                   maxpixels = maxpixels,
                                   main = title)
  # print(rastplot +polyplot)
  # browser()
  case_identifier <- 0
  if (!is.null(in_poly)) {
    polyplot  <- layer(sp.polygons(as(in_poly, "Spatial")))
    case_identifier <- case_identifier + 10
  } else {
    polyplot <- NULL
  }
# browser()
  if (!is.null(in_points)) {
    pointplot <- latticeExtra::layer(sp.points(as(in_points, "Spatial")))
    case_identifier <- case_identifier + 100
  } else {
    pointplot <- NULL
  }
# browser()
  if (plot_now) {
    if (case_identifier == 0)  print(rastplot)
    if (case_identifier == 10) print(rastplot + polyplot)
    if (case_identifier == 110) print(rastplot + polyplot + pointplot)
  }
}



# inrast <- system.file()


# plot_rast(in_rast, in_poly = in_vect)


#
# rastplot <- gplot(in_rast) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradient2(high   = highcol,
#                        mid    = midcol,
#                        low    = lowcol,
#                        limits = limits,
#                        midpoint = midpoint) +
#   xlab("X Coords") + ylab("Y Coords") +
#   geom_sf(data = prova,
#           colour = "black")+
#   coord_fixed() +
#   theme_bw()
#
# ggplot(data = prova) +
#   geom_sf(colour = "black")
#
#
# r <- raster(system.file("external/test.grd", package="raster"))
# s <- stack(r, r*2)
# names(s) <- c('meuse', 'meuse x 2')
#
# library(ggplot2)
#
# theme_set(theme_bw())
# gplot(s) + geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   scale_fill_gradient(low = 'white', high = 'blue') +
#   coord_equal()
