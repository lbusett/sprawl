context("Read a Raster file")

library(testthat)

test_that("read_rast works as expected", {
  # skip_on_travis()
  library(sprawl.data)
  # read single-band --> RasterLayer
  in_file <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
                         package = "sprawl.data")
  expect_is(read_rast(in_file), "RasterLayer")

  # read complete multi-band --> RasterBrick
  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  expect_is(read_rast(in_file), "RasterBrick")
  # read on band of multi-band --> RasterLayer
  expect_is(read_rast(in_file, bands = 2), "RasterLayer")

  # multiple bands of multi-band --> RasterStack
  expect_is(read_rast(in_file, bands = c(1,2)), "RasterStack")

  # multiple bands of multi-band vrt --> RasterStack
  # in_file <- system.file("extdata/MODIS_test", "OLI_test/oli_multi_1000_b1_b2.vrt",
  #                        package = "sprawl.data")
  # expect_is(read_rast(in_file, bands = c(1,2)), "RasterStack")

})
