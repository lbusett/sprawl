context("plot_rast")


test_that("plot_rast works as expected", {
  skip_on_travis()
  library(testthat)
  library(sprawl.data)
  in_file <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
                         package = "sprawl.data")
  in_rast <- read_rast(in_file)
  in_vect <- create_fishnet(in_rast, pix_for_cell = 150)
  # plot only the raster
  p <- plot_rast(in_rast, maxpixels = 100)
  expect_is(p, "NULL")
  p2 <- plot_rast(in_rast, in_poly = in_vect, maxpixels = 100)
  expect_is(p2, "NULL")

})
