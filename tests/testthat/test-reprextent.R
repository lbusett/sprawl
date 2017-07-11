context("Checking reprojection of extent")
testthat::test_that("Test extent_reproj",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  testthat::expect_error(extent_reproj(in_pts))

  in_rast  <- get(load(system.file("extdata", "sprawl_EVItest", package = "sprawl")))
  # no proj set
  testthat::expect_error(extent_reproj(extent(in_rast[[1]]), "+init=epsg:3035", "+init=epsg:432ds6"))

  testthat::expect_error(extent_reproj(extent(in_rast[[1]]), "+init=epsg:3sss035", "+init=epsg:432ds6"))

  # All Ok - `sp` Extent object
  testthat::expect_is(extent_reproj(raster::extent(in_rast[[1]]), "+init=epsg:3035", "+init=epsg:4326"), "Extent")

  # All Ok - `sf` bbox
  in_pts   <- readshape(system.file("extdata","randpoints.shp", package = "sprawl"))
  testthat::expect_is(extent_reproj(sf::st_bbox(in_pts), "+init=epsg:3035", "+init=epsg:4326"), "bbox")

  # no proj set
  testthat::expect_error(extent_reproj(in_pts, "+init=epsg:3035", "+init=epsg:4326"))


})
