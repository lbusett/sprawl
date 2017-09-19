context("get_vectype")

test_that("get_vectype works as expected", {
 skip_on_travis()
  library(sprawl.data)

  # input is a shapefile
  in_vect <- system.file("extdata/shapes","lc_polys.shp",
                         package = "sprawl.data")
  testthat::expect_equal(get_vectype(in_vect), "vectfile")

  # input is a `sp` object
  obj <- read_vect(in_vect, as_sp = TRUE)
  testthat::expect_equal(get_vectype(obj), "spobject")

  # input is a `sf` object
  obj <- read_vect(in_vect)
  testthat::expect_equal(get_vectype(obj),"sfobject")
  testthat::expect_equal(get_spatype(obj),"sfobject")
  # input is character but not a valid filename
  obj <- "xyzptr"
  testthat::expect_error(get_vectype(obj))

  # input is a valid filename but not a spatial file
  obj <- system.file("NEWS.md", package = "sprawl")
  obj <- system.file("NEWS.md", package = "sprawl")
  expect_error(out   <- get_vectype(obj))
  expect_error(out   <- get_spatype(obj))
  expect_warning(out <- get_vectype(obj, abort = FALSE))
  expect_equal(out, NA)
})
