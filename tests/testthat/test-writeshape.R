context("Writing a Shapefile")
test_that("Test writeshape", {
  # skip_on_cran()
  skip_on_travis()
  # input is a *sp object
  file = tempfile(fileext = ".shp")
  expect_warning(in_polys   <- build_testshape(30))
  expect_silent(writeshape(in_polys, tempfile(), verbose = FALSE))

  # input is a *sp object
  file = tempfile(fileext = ".shp")
  in_polys_sp <- as(in_polys, "sf")
  expect_silent(writeshape(in_polys_sp, tempfile(), verbose = FALSE))

  # output exists and overwrite is FALSE
  file = tempfile(fileext = ".shp")
  writeshape(in_polys, file, overwrite = FALSE, verbose = FALSE)
  expect_error(writeshape(in_polys, file, overwrite = FALSE, verbose = FALSE))

  # output exists and overwrite is TRUE
  expect_silent(writeshape(in_polys, file, overwrite = TRUE, verbose = FALSE))

  # input is not a *sp or *sf object
  expect_error(writeshape(in_rast, tempfile(), overwrite = TRUE, verbose = FALSE))
})

