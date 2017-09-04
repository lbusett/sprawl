context("plot_rast_gg")


test_that("plot_rast_gg works as expected", {

  skip_on_travis()
  in_rast <- read_rast(system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",
                                       package = "sprawl.data"))
  p <- plot_rast_gg(in_rast, scalebar_dist = 20)
  expect_is(p, "gg")

})
