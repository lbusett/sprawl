context("Get types of a spatial object/file")
testthat::test_that("get_spatype",{
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  # input is a raster file
  testthat::expect_equal(get_spatype(
    system.file("extdata/MODIS_test","EVIts_test.tif", package = "sprawl.data")),  #nolint
    "rastfile")

  # input is a shapefile
  in_vect <- system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data")  #nolint
  testthat::expect_equal(get_spatype(in_vect), "vectfile")

  # input is a `sp` object
  obj <- read_vect(in_vect, as_sp = TRUE)
  testthat::expect_equal(get_spatype(obj), "spobject")

  # input is a `sf` object
  obj <- read_vect(in_vect)
  testthat::expect_equal(get_spatype(obj),"sfobject")

  # input is character but not a valid filename
  obj <- "xyzptr"
  testthat::expect_error(get_spatype(obj, abort = TRUE))
  testthat::expect_warning(get_spatype(obj, abort = FALSE))

  # input is a valid filename but not a spatial file
  obj <- system.file("R","get_spatype.R", package = "sprawl")
  expect_error(out <- get_spatype(obj, abort = TRUE))
  expect_warning(out <- get_spatype(obj, abort = FALSE))
})

