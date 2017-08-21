context("Crop vector on a given extent")
testthat::test_that("Crop Vector object", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  in_polys       <- read_vect(system.file("extdata/shapes", "lc_polys.shp",
                                          package = "sprawl.data")
                              , stringsAsFactors = T)
  in_rast        <- raster::raster(system.file("extdata/MODIS_test", "EVIts_test.tif", #nolint
                                               package = "sprawl.data"))
  cropped_vect   <- crop_vect(in_polys, in_rast)
  expect_is(cropped_vect, "sf")

  # check errors in input selbands

})
