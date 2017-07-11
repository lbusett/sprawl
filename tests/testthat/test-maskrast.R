context("Masking a raster")
testthat::test_that("Test On raster masking", {
  # skip_on_cran()
  skip_on_travis()
  in_polys <- readshape(system.file("extdata","clip_shape.shp", package = "sprawl"), stringsAsFactors = T)
  in_rast  <- get(load(system.file("extdata", "sprawl_EVItest.RData", package = "sprawl")))[Q[1]]

  # in_raster <- build_testraster(1000,1000,1, to_file = TRUE)
  # in_poly   <- readshape(build_testshape(20, tofile = TRUE))
  # # check errors in input selbands
  maskrast(in_rast, in_polys)
  in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
  expect_warning(out <- maskrast(in_rast, in_polys))
  expect_is(out, "RasterLayer")

  # check that extracted values are equal to those of raster::extract
  # Check for the minimum and median of the differences (since `maskrast`
  # clips a little bit less than `raster::mask`
  sp_polys <- as(in_polys, "Spatial")
  out2 <- raster::mask(in_rast, sp_polys)
  a = raster::cellStats((out - out2), min)
  b = raster::cellStats((out - out2), mean)
  expect_equal(c(a,b), c(0,0) )

})
