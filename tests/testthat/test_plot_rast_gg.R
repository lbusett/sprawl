context("plot_rast_gg")


test_that("plot_rast_gg works as expected", {

  skip_on_travis()
  in_rast <- raster(nrows = 30, ncols = 30, crs = check_proj4string(3857),
                    xmn = 0, xmx = 3000, ymn = 0, ymx = 3000)
  in_rast <- setValues(in_rast, sample(10, 900, replace = T))

  # ,
  #                             crs = "+init=epsg:3857",
  #                             ext = raster::extent(c(0, 30000, 0, 30000)))
  p <- plot_rast_gg(in_rast,
                    palette_name = "RdYlGn",
                    zlims = c(0.05, 0.95),
                    zlims_type = "percs",
                    outliers_style = "to_minmax",
                    outliers_colors = c("blue", "yellow"),
                    rast_type = "continuous")
  expect_is(p, "gg")

  p <- plot_rast_gg(in_rast, scalebar_dist = 1,
                    palette_name = "hue",
                    zlims = c(0.05, 0.95),
                    zlims_type = "percs",
                    outliers_style = "recolor", leg_type = "discrete",
                    outliers_colors = c("blue", "yellow"),
                    rast_type = "categorical")

  expect_warning(p <- plot_rast_gg(in_rast, scalebar_dist = 1,
                    palette_name = "hue",
                    zlims = c(0.05, 0.95),
                    zlims_type = "percs",
                    outliers_style = "censor", leg_type = "discrete",
                    outliers_colors = c("blue", "yellow"),
                    borders_layer = create_fishnet(in_rast, pix_for_cell = 2)))

  expect_is(p, "gg")

})
