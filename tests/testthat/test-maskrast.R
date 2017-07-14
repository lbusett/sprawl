context("Masking a raster")
testthat::test_that("Test On raster masking", {
  # skip_on_cran()
  skip_on_travis()
  in_polys <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl"), stringsAsFactors = T)
  in_polys <- system.file("extdata","lc_polys.shp", package = "sprawl")
  in_rast  <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl"))[[1]]

  # check errors in input selbands
  masked   <- mask_rast(in_rast, in_polys, verbose = FALSE)
  in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
  out <- mask_rast(in_rast, in_polys, verbose = FALSE)
  expect_is(out, "RasterLayer")

  # check that extracted values are equal to those of raster::extract
  # Check for the minimum and median of the differences (since `mask_rast`
  # clips a little bit less than `raster::mask`
  sp_polys <- as(in_polys, "Spatial")
  out2 <- raster::mask(in_rast, sp_polys)
  a = raster::cellStats((out - out2), min)
  b = raster::cellStats((out - out2), mean)
  c = raster::cellStats((out - out2), max)
  expect_equal(c(a,b), c(0,0))

})
