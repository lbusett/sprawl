context("re-cast a vector object or vector file")
testthat::test_that("cast_vect",{
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)

  # input is a shapefile
  #
  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")
  expect_is(cast_vect(in_vect, "spobject"), "Spatial")

#
#
#   in_vect <- read_vect(in_vect)
#   cast_vect(in_vect, "sfobject")
#   cast_vect(in_vect, "spobject")
#   a = cast_vect(in_vect, "vectfile")
#   # input is a `sp` object
#   obj <- read_vect(system.file("extdata", "lc_polys.shp", package = "sprawl.data"), as_sp = TRUE)
#   testthat::expect_equal(get_spatype(obj), "spobject")
#
#   # input is a `sf` object
#   obj <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"))
#   testthat::expect_equal(get_spatype(obj),"sfobject")
#
#   # input is character but not a valid filename
#   obj <- "xyzptr"
#   testthat::expect_error(get_spatype(obj, abort = TRUE))
#   testthat::expect_warning(get_spatype(obj, abort = FALSE))
#
#   # input is a valid filename but not a spatial file
#   obj <- system.file("extdata", "sprawl_EVItest.RData", package = "sprawl.data")
#   expect_warning(out <- get_spatype(obj))
#   testthat::expect_equal(out,"none")
})

