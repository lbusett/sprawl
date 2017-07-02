context("Reading a Shapefile")
testthat::test_that("Test readshape", {
  # skip_on_cran()
  skip_on_travis()
  # open a shapefile as a `sf` object
  shp_file <- system.file("extdata","clip_shape.shp", package = "sprawl")
  shp      <- readshape(shp_file)
  testthat::expect_true(inherits(shp, "sf"))
  # open a shapefile as a `sp` object
  shp      <- readshape(shp_file, as_sp = TRUE)
  testthat::expect_true(inherits(shp, "SpatialPolygonsDataFrame"))

  # Try opening any other file
  testthat::expect_error(readshape(tempfile(fileext = ".shp"), as_sp = TRUE))
  })
