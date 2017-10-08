context("Aggregate raster on larger cells")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)

  # in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
  #                        package = "sprawl.data")
  # in_rast      <- read_rast(in_file, bands = 20)

  in_rast <- raster::raster(nrows = 50, ncols = 50) %>%
    raster::init("cell")
  tempraster   <- tempfile(fileext = ".tif")
  in_obj_zones <- raster::aggregate(in_rast,
                                    fact      = 4,
                                    filename  = tempraster,
                                    options   = "COMPRESS=DEFLATE",
                                    overwrite = T)

  test <- aggregate_rast(in_rast,
                         in_obj_zones,
                         FUN     = mean,
                         method  = "disk",
                         verbose = T)
  expect_is(test, "RasterLayer")
})
