context("plot_rast")


test_that("plot_rast works as expected", {
  skip_on_travis()
  # library(testthat)
  in_rast <- build_testraster(20,20)
  # plot only the raster
  p <- plot_rast(in_rast, maxpixels = 100)
  expect_is(p, "NULL")
  in_vect <- create_fishnet(in_rast)
  p2 <- plot_rast(in_rast, in_poly = in_vect, maxpixels = 100)
  expect_is(p2, "NULL")
})
