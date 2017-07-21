#'  in_rast <- system.file("extdata", "gNDVI.tif", package = "sprawl.data")
#'  in_poly <- create_fishnet(in_rast, pix_for_cell = 150)
#'  in_poly = as(in_vect, "Spatial")
#'  #'  # plot only the raster
#'  plot_rast(in_rast, in_poly = in_poly)



myplot <- function(in_rast, in_poly = NULL) {
  rastplot <- rasterVis::levelplot(in_rast,
                                   margin = FALSE)
  polyplot  <- layer(sp::sp.polygons(in_poly))
  rastplot + polyplot

}

library(rasterVis)
library(sf)
library(raster)

in_rast  <- raster(matrix(nrow = 1000, ncol = 1000))
in_rast  <- setValues(in_rast, seq(1:1000000))
my_poly  <- sprawl::create_fishnet(in_rast, pix_for_cell = 500)

my_poly  <- structure(list(cell_id = 1:4, geometry = structure(list(structure(list(
    structure(c(0, 0.5, 0.5, 0, 0, 0, 0, 0.5, 0.5, 0), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0.5, 1, 1, 0.5, 0.5, 0, 0, 0.5, 0.5, 0), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0, 0.5, 0.5, 0, 0, 0.5, 0.5, 1, 1, 0.5), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 1, 1, 0.5), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg"))), n_empty = 0L, class = c("sfc_POLYGON",
"sfc"), precision = 0, crs = structure(list(epsg = NA_integer_,
    proj4string = NA_character_), .Names = c("epsg", "proj4string"
), class = "crs"), bbox = structure(c(0, 0, 1, 1), .Names = c("xmin",
"ymin", "xmax", "ymax")))), .Names = c("cell_id", "geometry"), row.names = c(NA,
4L), class = c("sf", "data.frame"), sf_column = "geometry",
agr = structure(NA_integer_, class = "factor", .Label = c("constant",
"aggregate", "identity"), .Names = "cell_id"))

my_poly  <- as(my_poly, "Spatial")
myplot(in_rast, in_poly = my_poly)


in_poly <- my_poly
in_poly  <- as(in_poly, "Spatial")
myplot(in_rast, in_poly = in_poly)

in_poly <- structure(list(cell_id = 1:4, geometry = structure(list(structure(list(
    structure(c(0, 0.5, 0.5, 0, 0, 0, 0, 0.5, 0.5, 0), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0.5, 1, 1, 0.5, 0.5, 0, 0, 0.5, 0.5, 0), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0, 0.5, 0.5, 0, 0, 0.5, 0.5, 1, 1, 0.5), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(c(0.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 1, 1, 0.5), .Dim = c(5L,
    2L))), class = c("XY", "POLYGON", "sfg"))), n_empty = 0L, class = c("sfc_POLYGON",
"sfc"), precision = 0, crs = structure(list(epsg = NA_integer_,
    proj4string = NA_character_), .Names = c("epsg", "proj4string"
), class = "crs"), bbox = structure(c(0, 0, 1, 1), .Names = c("xmin",
"ymin", "xmax", "ymax")))), .Names = c("cell_id", "geometry"), row.names = c(NA,
4L), class = c("sf", "data.frame"), sf_column = "geometry",
agr = structure(NA_integer_, class = "factor", .Label = c("constant",
"aggregate", "identity"), .Names = "cell_id"))


in_poly = sprawl::create_fishnet(in_rast, pix_for_cell = 500)
in_poly  <- as(in_poly, "Spatial")
myplot(in_rast, in_poly = in_poly)



