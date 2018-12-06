# context("create_fishnet")

test_that("",{

  context("create_fishnet works as expected over raster")
  # skip_on_travis()
  in_rast  <-  build_testraster(30,30, crs = "+init=epsg:3857",
                                ext = raster::extent(c(0, 2000, 0, 2000)))
  # output of a fishnet is "sf"
  fishnet  <- create_fishnet(in_rast)
  expect_is(fishnet, "sf")
  # without cellsize all area is equal
  un_val <- unique(round(sf::st_area(fishnet), 5))
  expect_equal(length(un_val), 1)

  # with pix_for_cell we change n_aggregation
  fishnet  <- create_fishnet(in_rast, pix_for_cell = 2)
  un_val2 <- unique(round(sf::st_area(fishnet), 5))
  expect_equal(un_val2, un_val * 4)


  # using a cellsize can give different areas on the polys because some fall
  # outside the extent of the raster
  fishnet  <- create_fishnet(in_rast, cellsize = c(70,70))
  un_val3  <- unique(round(sf::st_area(fishnet), 5))
  expect_gt(length(un_val3), 1)

  context("create_fishnet works as expected over vector")
  in_vect <- read_vect(system.file("extdata/shapes","lc_polys.shp",
           package = "sprawl.data")) %>%
    sf::st_transform(3857)
  fishnet  <- create_fishnet(in_vect, cellsize = c(10000,10000))
  expect_equal(max(sf::st_area(fishnet) / 1e08), 1)

})
