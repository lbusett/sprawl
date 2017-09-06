context("Get types of a raster object/file")
testthat::test_that("get_rastype",{
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  # input is a raster file
  in_file <- system.file("extdata/MODIS_test","EVIts_test.tif", package = "sprawl.data")
  testthat::expect_equal(get_rastype(in_file), "rastfile")

  # input is a raster object
  in_rast <- read_rast(in_file)
  testthat::expect_equal(get_rastype(in_rast), "rastobject")

  # input is character but not a valid filename
  obj <- "xyzptr"
  testthat::expect_error(get_rastype(obj))

  # input is a valid filename but not a spatial file
  obj <- system.file("R","get_rastype.R", package = "sprawl")
  expect_error(out <- get_rastype(obj))

})

