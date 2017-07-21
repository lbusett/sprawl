context("Vector Operations - Crop vector")
testthat::test_that("Test On polygons extraction", {
  skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  in_polys       <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
  in_rast        <- get(load(system.file("extdata", "sprawl_EVItest.RData", package = "sprawl.data")))
  cropped_vect   <- crop_vect(in_polys, in_rast)
  expect_is(cropped_vect, "sf")

  # check errors in input selbands

})
