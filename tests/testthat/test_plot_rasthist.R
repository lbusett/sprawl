context("plot_rasthist")


test_that("plot_rasthist works as expected", {
  in_rast  <- build_testraster(20,40,5)
  expect_is(plot_rasthist(in_rast, type = "hist"), "gg")
  expect_is(plot_rasthist(in_rast, type = "line"), "gg")
  expect_is(plot_rasthist(in_rast, variable = "count"), "gg")

})
