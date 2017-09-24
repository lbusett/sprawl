context("plot_rast_gg")


test_that("plot_rast_gg works as expected", {

  skip_on_travis()
  in_rast <- build_testraster(30,30,
                              crs = "+init=epsg:3857",
                              ext = raster::extent(c(0, 200000, 0, 200000)))
  p <- plot_rast_gg(in_rast, scalebar_dist = 10, palette_type = "diverging",
                    palette_name = "RdYlGn",
                    zlims = c(0.05, 0.95),
                    zlims_type = "percs",
                    outliers_style = "as_na",
                    outliers_color = c("blue", "yellow"))
  expect_is(p, "gg")

})
