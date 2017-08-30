context("Read a Raster file")

library(testthat)
test_that("read_rast works as expected", {

in_file <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
                           package = "sprawl.data")
expect_is(read_rast(in_file), "Raster")

in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                           package = "sprawl.data")
expect_is(read_rast(in_file), "RasterBrick")

in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                           package = "sprawl.data")
expect_is(read_rast(in_file, bands_to_read = c(1,2)), "RasterStack")


})
