plot_rast <- function(in_rast,
                          highcol  = "darkgreen",
                          midcol   = "yellow",
                          lowcol   = "darkred",
                          limits   = NULL,
                          midpoint = NULL) {

  if(is.null(limits)) {
    limits <- raster::quantile(in_rast, probs = c(0, 0.01, 0.99, 1), ncells = 100e5)
    # limits <- quants[c(1,2)]
    if (is.null(midpoint)) {
      midpoint <- mean(quants, na.rm = TRUE)
    }
  }
  rastplot <- gplot(in_rast) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(high   = highcol,
                         mid    = midcol,
                         low    = lowcol,
                         limits = limits,
                         midpoint = midpoint) +
    xlab("X Coords") + ylab("Y Coords") +
    geom_sf(data = prova,
                 colour = "black")+
      coord_fixed() +
      theme_bw()

  ggplot(data = prova) +
    geom_sf(colour = "black")


  r <- raster(system.file("external/test.grd", package="raster"))
  s <- stack(r, r*2)
  names(s) <- c('meuse', 'meuse x 2')

  library(ggplot2)

  theme_set(theme_bw())
  gplot(s) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_gradient(low = 'white', high = 'blue') +
    coord_equal()

  prova <- create_fishnet(in_rast, pix_for_cell = 100 )

  a = rasterVis::levelplot(in_rast,
            margin = FALSE,
            col.regions = my.col,
            at = c(limits[1], seq(limits[2], limits[3], (limits[3] - limits[2])/48), limits[4]),
            # at = c(seq(limits[2], limits[3], (limits[3] - limits[2])/50)),
            maxpixels = 1e5, main = title)
  a + layer(sp.polygons(as(prova, "Spatial")))

  # my.palette <- brewer.pal(n = 25, name = "RdYlGn")
  my.col <- c("grey10", colorRampPalette(brewer.pal(11, "RdYlGn"))(48), "purple")
  my.col <- c( colorRampPalette(brewer.pal(11, "RdYlGn"))(50))
}
