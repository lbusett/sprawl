context("Write a Shapefile")
test_that("Test write_shape", {
  # skip_on_cran()
  skip_on_travis()

  in_file  <- system.file("extdata/shapes","randpoints.shp",
                          package = "sprawl.data")
  # input is a *sf object
  in_pts   <- read_vect(in_file)
  expect_silent(write_shape(in_pts, tempfile(fileext = ".shp"),
                            verbose = FALSE))

  # input is a *sp object
  out_file <- tempfile(fileext = ".shp")
  in_pts   <- read_vect(in_file, as_sp = TRUE)
  expect_silent(write_shape(in_pts, out_file , verbose = FALSE))

  # output exists and overwrite is FALSE
  expect_error(write_shape(in_pts, out_file , verbose = FALSE))

  # output exists and overwrite is TRUE
  expect_silent(write_shape(in_pts, out_file , overwrite = TRUE,
                         verbose = FALSE))

})

