in_rast     <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
in_vect   <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
in_polys <- as(in_vect, "Spatial")  %>%
  spTransform(get_projstring(in_rast))


tmprast <- tempfile(fileext = ".tif")
in_rast <- build_testraster(2000, 7000, 12) %>%
  writeRaster(filename = tmprast)
in_vect <- create_fishnet(in_rast, pix_for_cell = 500) %>%
  dplyr::sample_n(1)
in_vect <- sf::st_set_crs(in_vect, get_projstring(in_rast))
sp_polys <- as(in_vect, "Spatial")

bench <- microbenchmark::microbenchmark(
  raster = {out_mask_raster  <- raster::mask(in_rast, in_polys)},
  sprawl = {out_mask_raster  <- mask_rast(in_rast, in_vect, crop = TRUE)},
  times = 5)
