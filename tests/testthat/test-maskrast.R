context("Masking a raster")
testthat::test_that("Test On raster masking", {
  skip_on_cran()
  skip_on_travis()

  library(sprawl.data)
  # both raster and mask are "R" objects - check if works and equal to raster::mask ----
  mask_vect   <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
  in_rast     <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))[[1]]
  # test both with same projection and differenrt projections between in_rast and mask_vect
  masked      <- mask_rast(in_rast, mask_vect, verbose = FALSE, crop = FALSE)
  expect_is(masked, "RasterLayer")
  mask_vect   <- sf::st_transform(mask_vect, proj4string(in_rast))
  out         <- mask_rast(in_rast, mask_vect, verbose = FALSE)
  expect_is(out, "RasterLayer")
  expect_equal(masked, out)

  # Save to file ----
  masked_file <- mask_rast(in_rast, mask_vect, to_file = TRUE, verbose = FALSE, crop = T)
  expect_is(masked_file, "character")

  # check that extracted values are equal to those of raster::extract ---
  # Check for all zero on the difference of the two since sprawl::mask
  # keeps a bit more pixels
  sp_polys    <- mask_vect %>%
    sf::st_transform(as.character(proj4string(in_rast))) %>%
    as("Spatial")
  out2        <- raster::mask(in_rast, sp_polys)
  expect_equal(unique(getValues(out - out2)), c(NA, 0))

  # both raster and mask are filenames - check if it works ----
  mask_vect    <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
  in_rast     <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
  # check errors in input selbands
  masked2      <- mask_rast(in_rast, mask_vect, verbose = FALSE)
  expect_is(masked, "RasterLayer")
  expect_equal(unique(getValues(masked - masked2)), c(NA, 0))

})
