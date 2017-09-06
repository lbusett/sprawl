context("create_fishnet")


test_that("create_fishnet works as expected", {
  skip_on_travis()
  in_rast  <-  build_testraster(30,30, crs = "+init=epsg:3857",
                              ext = raster::extent(c(0, 2000, 0, 2000)))
  # output of a fishnet is "sf"
  fishnet  <- create_fishnet(in_rast)
  expect_is(fishnet, "sf")

  # using a cellsize can give different areas on the polys
  fishnet  <- create_fishnet(in_rast, cellsize = c(70,70))
  expect_lt(max(diff(unique(sf::st_area(fishnet)))), 0)

  # unless using also `exact_csize`
  fishnet  <- create_fishnet(in_rast, cellsize = c(70,70), exact_csize = FALSE)
  expect_equal(max(diff(unique(sf::st_area(fishnet)))), 0)

})
