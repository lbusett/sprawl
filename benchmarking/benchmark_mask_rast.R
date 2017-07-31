in_rast     <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
in_vect   <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
sp_polys <- as(in_vect, "Spatial")  %>%
  sp::spTransform(get_projstring(in_rast))


tmprast <- tempfile(fileext = ".tif")
in_rast <- build_testraster(1000, 1000, 5) %>%
  raster::writeRaster(filename = tmprast)
in_vect <- create_fishnet(in_rast, pix_for_cell = 200) %>%
  dplyr::sample_n(20)
in_vect <- sf::st_set_crs(in_vect, get_projstring(in_rast))
sp_polys <- as(in_vect, "Spatial")

bench <- microbenchmark::microbenchmark(
  "raster::mask" = {out_mask_raster  <- raster::mask(in_rast, sp_polys)},
  "sprawl::mask_rast" = {out_mask_raster  <- mask_rast(in_rast, in_vect, crop = FALSE)},
  times = 5)
boxplot(bench)
