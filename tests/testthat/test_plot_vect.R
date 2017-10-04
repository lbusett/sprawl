context("plot_vect")


test_that("plot_vect works as expected", {
  library(ggplot2)
  in_vect <- get(load(system.file("extdata/shapes", "poly_lomb.RData",
                                  package = "sprawl.data")))
  # plot only geometry
  expect_warning(p <- plot_vect(in_vect))
  expect_is(p, "gg")

  expect_warning(p <- plot_vect(in_vect, line_color = "blue", line_size = 1.5))
  expect_is(p, "gg")
  # plot with a fill on a cartegorical variable with a single "level"
  p <- plot_vect(in_vect, fill_var = "NAME_2")
  expect_is(p, "gg")

  # change the palette, add a scalebar and remove the grid
  expect_is(plot_vect(in_vect, fill_var = "NAME_2",no_axis = F, palette_name = "Set3",
            scalebar = TRUE, grid = FALSE), "gg")


  # plot with a fill on a continuous variable with two "levels", using facets
  # and a diverging palette. also add a "borders" layer with a different
  #plot color
  expect_is(plot_vect(in_vect, fill_var = "population", facet_var = "year",
            palette = "RdYlBu", scalebar = T, scalebar_dist = 50,
            grid = FALSE, zlims = c(5,20), outliers_colors = c("yellow", "green"),
            borders_layer = get_boundaries("ITA", level = 0),
            borders_color = "red"), "gg")

  expect_warning(plot_vect(in_vect,
                         palette_name = "9r38w09r83",
                         fill_var = "NAME_2"))

})
