context("Aggregate raster on larger cells")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  # skip_on_travis()
  library(sprawl.data)

  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                                          package = "sprawl.data")
  in_rast      <- read_rast(in_file, bands = 20)
  tempraster   <- tempfile(fileext = ".tif")
  in_obj_zones <- raster::aggregate(in_rast,
                                      fact      = 4,
                                      filename  = tempraster,
                                      overwrite = T)

  expect_warning(test <- aggregate_rast(in_rast,
                                        in_obj_zones,
                                        FUN     = mean,
                                        method  = "fastdisk",
                                        to_file = FALSE,
                                        verbose = FALSE))
  expect_is(test, "RasterLayer")
})
