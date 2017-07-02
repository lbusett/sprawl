context("Masking a raster")
testthat::test_that("Test On raster masking", {
  # skip_on_cran()
  skip_on_travis()
  in_polys <- readshape(system.file("extdata","lc_polys.shp", package = "sprawl"), stringsAsFactors = T)
  in_rast  <- in_rts[[1]]

  # check errors in input selbands
  expect_error(maskrast(in_rast, in_polys))  # TODO : automatic reprojection of the clipper !!!
  in_polys <- st_transform(in_polys, proj4string(in_rast))
  expect_warning(out <- maskrast(in_rast, in_polys))
  expect_is(out, "RasterLayer")

  # check that extracted values are equal to those of raster::extract
  # Check for the minimum and median of the differences (since `maskrast`
  # clips a little bit less than `raster::mask`
  out2 <- mask(in_rast, as(in_polys, "Spatial"))
  a = raster::cellStats((out - out2), min)
  b = raster::cellStats((out - out2), mean)
  expect_equal(c(a,b), c(0,0) )

})
