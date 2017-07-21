context("Checking types of spatial objects")
testthat::test_that("check_spatype",{
  skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  # input is a raster file
  testthat::expect_equal(check_spatype(system.file("extdata","sprawl_EVItest.tif", package = "sprawl.data")),
                         "rastfile")

  # input is a shapefile
  testthat::expect_equal(check_spatype(system.file("extdata","lc_polys.shp", package = "sprawl.data")),
                         "vectfile")

  # input is a `sp` object
  obj <- read_shape(system.file("extdata", "lc_polys.shp", package = "sprawl.data"), as_sp = TRUE)
  testthat::expect_equal(check_spatype(obj), "spobject")

  # input is a `sf` object
  obj <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl.data"))
  testthat::expect_equal(check_spatype(obj),"sfobject")

  # input is not a spatial object
  obj <- "pippo"
  testthat::expect_equal(check_spatype(obj),"none")

})

