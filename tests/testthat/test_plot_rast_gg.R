context("plot_rast_gg")


test_that("plot_rast_gg works as expected", {

  library(raster)
  library(magrittr)
  skip_on_travis()
  in_rast <- raster(nrows = 30, ncols = 30, crs = check_proj4string(3857),
                    xmn = 0, xmx = 3000, ymn = 0, ymx = 3000)
  in_rast <- setValues(in_rast, sample(10, 900, replace = T))

  p <- plot_rast_gg(in_rast,
                    palette_name = "RdYlGn",
                    zlims = c(2, 9),
                    zlims_type = "vals",
                    outliers_style = "censor",
                    outliers_colors = c("blue", "yellow"),
                    rast_type = "continuous")
  expect_is(p, "gg")
  expect_silent(p)

  p <- plot_rast_gg(in_rast,
                    palette_name = "hue",
                    zlims = c(0.05, 0.95),
                    zlims_type = "percs",
                    outliers_style = "recolor", leg_type = "discrete",
                    outliers_colors = c("blue", "yellow"),
                    rast_type = "categorical")

  expect_is(p, "gg")
  expect_silent(p)

  p <- plot_rast_gg(in_rast,
                    palette_name = "hue",
                    zlims = c(0.20, 0.90),
                    zlims_type = "percs",
                    outliers_style = "censor", leg_type = "discrete",
                    outliers_colors = c("blue", "yellow"),
                    borders_layer = create_fishnet(in_rast,
                                                   pix_for_cell = 2))
  expect_is(p, "gg")
  expect_silent(p)

  expect_warning(
    p <- plot_rast_gg(
      in_rast,
      palette_name = "Greens",
      zlims = c(0.05, 0.95),
      zlims_type = "percs",
      outliers_style = "censor", leg_type = "discrete",
      outliers_colors = c("blue", "yellow"),
      borders_layer = create_fishnet(in_rast, pix_for_cell = 2))
  )

  expect_is(p, "gg")
  expect_silent(p)

  # Categorical raster - manual scale
  in_rast <- raster::raster(ncol = 20, nrow = 20) %>%
    raster::init("row") %>%
    set_rastlabels(class_names = letters[1:20])


  p <- plot_rast_gg(in_rast,  palette_name = "manual",
                    leg_colors = c("red"))
  expect_is(p, "gg")
  expect_error(print(p))

  p <- plot_rast_gg(in_rast,  palette_name = "manual",
                    leg_colors = rainbow(20))
  expect_is(p, "gg")
  expect_silent(p)

})
