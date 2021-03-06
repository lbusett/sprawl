testthat::test_that("Test On raster aggregation", {
  context("aggregate_rast --> Aggregate raster on larger cells")
  # skip_on_cran()
  # skip_on_travis()
  library(sprawl.data)

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
                         verbose = F)
  expect_is(test, "RasterLayer")
})
