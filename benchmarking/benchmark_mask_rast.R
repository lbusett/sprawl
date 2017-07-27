in_rast     <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
mask_vect   <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
in_polys <- as(mask_vect, "Spatial")

bench <- microbenchmark::microbenchmark(
  raster = {out_mask_raster  <- raster::mask(in_rast, sp_polys)},
  sprawl = {out_mask_raster  <- rast_mask(in_rast, mask_vect)},
  times = 5)
