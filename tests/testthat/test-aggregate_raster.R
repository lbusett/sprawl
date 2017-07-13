context("Raster aggregation")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  skip_on_travis()

  library(raster)
  library(dplyr)

  in_rast_values = raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl"))[[20]]
  tempraster <- tempfile(fileext = ".tif")
  in_obj_zones   = in_rast_values %>%
    raster::aggregate(fact = 4, filename = tempraster,
                      overwrite = T)

  # raster::writeRaster(in_obj_zones, file = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\sprawl_EVItest_agg.tif",
  #                     overwrite = T)
  expect_warning(test = aggregate_rast(in_rast_values,
                 in_obj_zones,
                 FUN = mean,
                 method = "fastdisk",
                 to_file = FALSE,
                 verbose = FALSE))
  expect_is(test, "RasterLayer")
})
#
# profvis::profvis({test = aggregate_rast(in_rast_values,
#                                         in_obj_zones,
#                                         FUN = mean,
#                                         method = "fastdisk",
#                                         to_file = TRUE,
#                                         out_file = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\testaggr.tif",
#                                         verbose = FALSE)}, torture = 1, interval = 0.005)
