context("Checking types of spatial objects")
testthat::test_that("check_spatype",{
  # skip_on_cran()
  skip_on_travis()
  # input is a raster file
  testthat::expect_equal(check_spatype(system.file("extdata","testrast.tif", package = "sprawl")),
                         "rastfile")

  # input is a shapefile
  testthat::expect_equal(check_spatype(system.file("extdata","lc_polys.shp", package = "sprawl")),
                         "vectfile")

  # input is a `sp` object
  obj <- read_shape(system.file("extdata", "lc_polys.shp", package = "sprawl"), as_sp = TRUE)
  testthat::expect_equal(check_spatype(obj), "spobject")

  # input is a `sf` object
  obj <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl"))
  testthat::expect_equal(check_spatype(obj),"sfobject")

  # input is not a spatial object
  obj <- "pippo"
  testthat::expect_equal(check_spatype(obj),"none")

})

