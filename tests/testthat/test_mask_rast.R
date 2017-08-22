context("Mask a raster on features of a vector")
testthat::test_that("Test On raster masking", {
  # skip_on_cran()
  skip_on_travis()
  library(testthat)
  library(sprawl.data)

  mask_vect <- system.file("extdata/shapes", "lc_polys.shp",
                           package = "sprawl.data")
  in_rast   <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                           package = "sprawl.data")
  # both raster and mask are "R" objects - check if works and equal to
  # raster::mask ----
  mask_in <- read_vect(mask_vect, stringsAsFactors = T)
  rast_in <- raster::stack(in_rast)[[5]]
  # test both with same projection and differenrt projections between in_rast
  #  and mask_vect
  out_masked   <- mask_rast(rast_in, mask_vect, verbose = FALSE, crop = FALSE)
  expect_is(out_masked, "Raster")
  mask_in_2    <- sf::st_transform(mask_in, get_proj4string(rast_in))
  out_masked_2 <- mask_rast(rast_in, mask_in_2, verbose = FALSE)
  expect_is(out_masked_2, "Raster")
  expect_equal(raster::getValues(out_masked),
               raster::getValues(out_masked_2))

  # Save to file and crop----
  masked_file <- mask_rast(rast_in, mask_in,
                           out_type = "filename", verbose = FALSE, crop = T)
  expect_is(masked_file, "character")
  masked_file_multi <- mask_rast(rast_in, mask_in, verbose = FALSE, crop = T)
  expect_is(masked_file, "character")
  expect_is(masked_file_multi, "Raster")

  # check that extracted values are equal to those of raster::extract ---
  # Check for all zero on the difference of the two since sprawl::mask
  # keeps a bit more pixels
  sp_polys    <- mask_in %>%
    sf::st_transform(as.character(sp::proj4string(rast_in))) %>%
    as("Spatial")
  out_mask_raster  <- raster::mask(rast_in, sp_polys)
  diff <- unique(raster::getValues(out_masked) - raster::getValues(out_mask_raster))  #nolint
  expect_equal(as.numeric(diff), c(NA, 0))


  # both raster and mask are filenames - check if it works ----
  in_rast   <- system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",
                           package = "sprawl.data")
  mask_vect <- system.file("extdata/shapes", "oli_polys.shp",
                           package = "sprawl.data")
  masked_fromfiles   <- mask_rast(in_rast, mask_vect, crop = TRUE,
                                  verbose = FALSE)
  expect_is(masked_fromfiles, "Raster")

})
