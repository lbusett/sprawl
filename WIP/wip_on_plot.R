

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

