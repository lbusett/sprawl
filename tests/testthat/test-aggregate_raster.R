context("Raster aggregation")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  skip_on_travis()

  library(raster)
  library(dplyr)
  library(sprawl.data)

  in_rast_values <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))[[20]]
  tempraster     <- tempfile(fileext = ".tif")
  in_obj_zones   <- raster::aggregate(in_rast_values,
                                      fact = 4,
                                      filename = tempraster,
                                      overwrite = T)

  expect_warning(test <- aggregate_rast(in_rast_values,
                                        in_obj_zones,
                                        FUN = mean,
                                        method = "fastdisk",
                                        to_file = FALSE,
                                        verbose = FALSE))
  expect_is(test, "RasterLayer")
})
