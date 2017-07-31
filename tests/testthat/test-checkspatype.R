context("Checking types of spatial objects")
testthat::test_that("get_spatype",{
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  # input is a raster file
  testthat::expect_equal(get_spatype(system.file("extdata","sprawl_EVItest.tif", package = "sprawl.data")),
                         "rastfile")

  # input is a shapefile
  testthat::expect_equal(get_spatype(system.file("extdata","lc_polys.shp", package = "sprawl.data")),
                         "vectfile")

  # input is a `sp` object
  obj <- read_vect(system.file("extdata", "lc_polys.shp", package = "sprawl.data"), as_sp = TRUE)
  testthat::expect_equal(get_spatype(obj), "spobject")

  # input is a `sf` object
  obj <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"))
  testthat::expect_equal(get_spatype(obj),"sfobject")

  # input is character but not a valid filename
  obj <- "xyzptr"
  testthat::expect_error(get_spatype(obj, abort = TRUE))
  testthat::expect_warning(get_spatype(obj, abort = FALSE))

  # input is a valid filename but not a spatial file
  obj <- system.file("extdata", "sprawl_EVItest.RData", package = "sprawl.data")
  expect_warning(out <- get_spatype(obj))
  testthat::expect_equal(out,"none")
})

