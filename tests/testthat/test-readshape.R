context("Reading a Shapefile")
testthat::test_that("Test read_shape", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  # open a shapefile as a `sf` object
  shp_file <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
  shp      <- read_shape(shp_file)
  testthat::expect_true(inherits(shp, "sf"))
  # open a shapefile as a `sp` object
  shp      <- read_shape(shp_file, as_sp = TRUE)
  testthat::expect_true(inherits(shp, "SpatialPolygonsDataFrame"))

  # Try opening any other file
  testthat::expect_error(read_shape(tempfile(fileext = ".shp"), as_sp = TRUE))
})
