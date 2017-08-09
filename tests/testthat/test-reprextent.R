context("Checking reprojection of extent")
testthat::test_that("Test reproj_extent",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  testthat::expect_error(reproj_extent(in_pts))

  in_rast  <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
  # no proj set
  testthat::expect_error(reproj_extent(extent(in_rast[[1]]), "+init=epsg:3035", "+init=epsg:432ds6"))

  testthat::expect_error(reproj_extent(extent(in_rast[[1]]), "+init=epsg:3sss035", "+init=epsg:432ds6"))

  # All Ok - `sp` Extent object
  testthat::expect_is(reproj_extent(raster::extent(in_rast[[1]]), "+init=epsg:3035", "+init=epsg:4326"), "Extent")

  # All Ok - `sf` bbox
  in_pts   <- read_vect(system.file("extdata","randpoints.shp", package = "sprawl.data"))
  testthat::expect_is(reproj_extent(sf::st_bbox(in_pts), "+init=epsg:3035", "+init=epsg:4326"), "bbox")

  # no proj set
  testthat::expect_error(reproj_extent(in_pts, "+init=epsg:3035", "+init=epsg:4326"))


})
