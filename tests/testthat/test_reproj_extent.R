context("Reproject an extent")
testthat::test_that("Test reproj_extent",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  testthat::expect_error(reproj_extent(in_pts))

  in_rast  <- raster::stack(system.file("extdata/MODIS_test", "EVIts_test.tif",
                                        package = "sprawl.data"))
  # no proj set
  testthat::expect_error(reproj_extent(extent(in_rast[[1]]),
                                       "+init=epsg:3035", "+init=epsg:432ds6"))

  testthat::expect_error(reproj_extent(extent(in_rast[[1]]),
                                       "+init=epsg:3sss035", "+init=epsg:432ds6"))  #nolint

  # All Ok - `sp` Extent object
  testthat::expect_is(reproj_extent(raster::extent(in_rast[[1]]),
                                    "+init=epsg:3035", "+init=epsg:4326"), "Extent")  #nolint

  # All Ok - `sf` bbox
  in_pts   <- read_vect(system.file("extdata/shapes","randpoints.shp",
                                    package = "sprawl.data"))
  testthat::expect_is(reproj_extent(sf::st_bbox(in_pts),
                                    "+init=epsg:3035", "+init=epsg:4326"), "bbox")  #nolint

  # no proj set
  testthat::expect_error(reproj_extent(in_pts, "+init=epsg:3035", "+init=epsg:4326"))  #nolint


})
