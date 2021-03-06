

test_that("cast_vect works as expected", {
  # library(testthat)
  # skip_on_travis()
  # skip_on_cran()
  library(sprawl.data)

  # input is a shapefile
  context("cast_vect -> OK on file")
  in_file <- system.file("extdata/shapes","lc_polys.shp",
                         package = "sprawl.data")
  expect_is(cast_vect(in_file, "spobject"), "Spatial")
  expect_is(cast_vect(in_file, "sfobject"), "sf")
  expect_is(cast_vect(in_file, "vectfile"), "character")

  # input is a sf object
  context("cast_vect -> OK on sf")
  in_sf <- read_vect(in_file)
  expect_is(cast_vect(in_sf, "spobject"), "Spatial")
  expect_is(cast_vect(in_sf, "sfobject"), "sf")
  expect_is(cast_vect(in_sf, "vectfile"), "character")

  # input is a sp object
  context("cast_vect -> OK on sp")
  in_sp <- read_vect(in_file, as_sp = TRUE)
  expect_is(cast_vect(in_sp, "spobject"), "Spatial")
  expect_is(cast_vect(in_sp, "sfobject"), "sf")
  expect_is(cast_vect(in_sp, "vectfile"), "character")

  # bad inputs
  context("cast_vect -> Error on invalid file name")
  expect_error(cast_vect("nofile", "spobject"))
  #TODO test on non-spatial file
  #expect_error(cast_vect("nofile", "spobject"))


})
