context("Cropping a raster")
testthat::test_that("Test On raster cropping", {
  # skip_on_cran()
  skip_on_travis()
  library(testthat)
  library(sprawl.data)
  # both raster and mask are "R" objects - check if works and equal to raster::mask ----
  in_rast <- raster::stack(system.file("extdata/OLI_test", "oli_multi.tif", package = "sprawl.data"))
  in_vect <- create_fishnet(in_rast, pix_for_cell = 60)[50,]
  # test both with same projection and differenrt projections between in_rast and mask_vect
  out_cropped  <- crop_rast(in_rast, in_vect, verbose = FALSE, out_type = "rastobject")
  out_vrt      <- crop_rast(in_rast, in_vect, verbose = FALSE, out_type = "vrtfile")
  out_rastfile <- crop_rast(in_rast, in_vect, verbose = FALSE, out_type = "rastfile")
  expect_is(out_cropped, "Raster")
  expect_is(out_vrt, "character")
  expect_is(out_rastfile, "character")
  expect_is(raster::brick(out_vrt), "Raster")
  expect_is(raster::brick(out_rastfile), "Raster")
  expect_equal(as.numeric(getValues(raster::brick(out_vrt))),
               as.numeric(getValues(raster::brick(out_rastfile))))
  expect_error(crop_rast("ciao", in_vect))
  expect_error(crop_rast(in_rast, "ciao.shp"))

})
