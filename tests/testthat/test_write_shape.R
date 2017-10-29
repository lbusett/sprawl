test_that("Test write_shape", {
  context("Write a Shapefile")
  # skip_on_cran()
  skip_on_travis()

  in_file  <- system.file("extdata/shapes","randpoints.shp",
                          package = "sprawl.data")
  # input is a *sf object
  in_pts   <- read_vect(in_file)
  outfile  <- tempfile(fileext = ".shp")
  write_shape(in_pts, outfile, verbose = FALSE)
  assertthat::is.readable(outfile)
  # input is a *sp object
  out_file <- tempfile(fileext = ".shp")
  in_pts   <- read_vect(in_file, as_sp = TRUE)
  write_shape(in_pts, out_file , verbose = FALSE)
  assertthat::is.readable(outfile)
  # output exists and overwrite is FALSE
  expect_error(write_shape(in_pts, out_file , verbose = FALSE))

  # output exists and overwrite is TRUE
  write_shape(in_pts, out_file , overwrite = TRUE,
                         verbose = FALSE)
  assertthat::is.readable(outfile)

  context("auto renaming of columns")

  names(in_pts)[2] = "longnamenospaces"
  out_file <- tempfile(fileext = ".shp")
  expect_warning(write_shape(in_pts, out_file, overwrite = T))
  reload <- read_vect(out_file)
  expect_equal(names(reload)[2], "lngnmnspcs")

  expect_warning(names(in_pts)[2] <- "longnameno w spaces")
  expect_warning(write_shape(in_pts, out_file, overwrite = T))
  reload <- read_vect(out_file)
  expect_equal(names(reload)[2], "lngnmn_w_s")

})

