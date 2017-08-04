context("Masking a raster")
testthat::test_that("Test On raster masking", {
  # skip_on_cran()
  skip_on_travis()
  library(testthat)
  library(sprawl.data)
  # both raster and mask are "R" objects - check if works and equal to raster::mask ----
  mask_vect   <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
  in_rast     <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
  # test both with same projection and differenrt projections between in_rast and mask_vect
  out_masked   <- mask_rast(in_rast, mask_vect, verbose = FALSE, crop = FALSE)
  expect_is(out_masked, "Raster")
  mask_vect    <- sf::st_transform(mask_vect, sp::proj4string(in_rast))
  out_masked_2 <- mask_rast(in_rast, mask_vect, verbose = FALSE)
  expect_is(out_masked_2, "Raster")
  expect_equal(raster::getValues(out_masked),
               raster::getValues(out_masked_2))

  # Save to file and crop----
  masked_file <- mask_rast(in_rast, mask_vect, out_type = "filename", verbose = FALSE, crop = T)
  expect_is(masked_file, "character")
  masked_file_multi <- mask_rast(in_rast, mask_vect,
                                 verbose = FALSE, crop = T, save_multiband = TRUE)
  expect_is(masked_file, "character")
  expect_is(masked_file_multi, "Raster")

  # check that extracted values are equal to those of raster::extract ---
  # Check for all zero on the difference of the two since sprawl::mask
  # keeps a bit more pixels
  sp_polys    <- mask_vect %>%
    sf::st_transform(as.character(sp::proj4string(in_rast))) %>%
    as("Spatial")
  out_mask_raster  <- raster::mask(in_rast, sp_polys)
  diff <- unique(raster::getValues(out_masked) - raster::getValues(out_mask_raster))
  expect_equal(as.numeric(diff), c(NA, 0))


  # both raster and mask are filenames - check if it works ----
  mask_vect <- system.file("extdata", "lc_polys.shp", package = "sprawl.data")
  in_rast   <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
  # check errors in input selbands
  masked_fromfiles   <- mask_rast(in_rast, mask_vect, verbose = FALSE)
  expect_is(masked_fromfiles, "Raster")
  # expect_equal(unique(raster::getValues(out_masked - masked_fromfiles)), c(0))

})
