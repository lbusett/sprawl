context("get_raststats")


test_that("get_raststats works as expected", {

  library(raster)
  in_rast <- raster::raster(nrows = 50, ncols = 50) %>%
    raster::init("cell")
  in_stack <- raster::stack(in_rast, in_rast)
  # calling with different args gives expected elements of the list
  stats = get_raststats(in_stack)
  expect_is(stats$stats, "data.frame")
  expect_equal(stats$stats$max, c(2500,2500))
  expect_equal(stats$stats$avg, c(1250.5, 1250.5))
  expect_is(get_raststats(in_rast, quantiles = T)$quants, "data.frame")
  stats = get_raststats(in_rast, hist = T)
  expect_is(stats$hists, "data.frame")

  # Maximum cumfreq is always one
  expect_equal(max(stats$hist$cumfreq), 1)
})
