context("Reproject an extent")
testthat::test_that("Test reproj_extent",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  testthat::expect_error(reproj_extent(in_pts))

  in_rast  <- raster::stack(system.file("extdata/MODIS_test", "EVIts_test.tif",
                                        package = "sprawl.data"))
  # TODO replace with gird_it for showing the difference between enlarge=TRUE and FALSE

  # All ok - reproj from sprawlext
  in_sprawlext_1 <- get_extent(in_rast)
  ext_reproj_1 <- reproj_extent(in_sprawlext_1, "+init=epsg:32651")
  testthat::expect_is(ext_reproj_1, "sprawlext")

  # All ok - reproj from raster input
  ext_reproj_2 <- reproj_extent(in_rast, "+init=epsg:32651")
  testthat::expect_is(ext_reproj_2, "sprawlext")
  testthat::expect_equal(ext_reproj_2, ext_reproj_1)

  # All ok - reproj from extent and in_proj
  ext_reproj_3 <- reproj_extent(extent(in_rast), "+init=epsg:32651", in_proj=in_rast@crs)
  testthat::expect_is(ext_reproj_3, "sprawlext")
  testthat::expect_equal(ext_reproj_3, ext_reproj_1)

  # All ok - reproj without enlarging
  ext_reproj_4 <- reproj_extent(in_sprawlext_1, "+init=epsg:32651", enlarge = FALSE)
  expect_equal(st_covers(as(ext_reproj_1,"sfc_POLYGON"), as(ext_reproj_4,"sfc_POLYGON"), sparse=FALSE),
               matrix(TRUE))

  # Error - no proj correctly set
  testthat::expect_error(
    reproj_extent(in_sprawlext_1, "+init=epsg:432ds6"))
  testthat::expect_error(
    reproj_extent(extent(in_rast), "+init=epsg:432ds6",
                  in_proj = "+init=epsg:3sss035"))  #nolint

  # Error - given bounding box but missing in_proj
  testthat::expect_error(
    reproj_extent(bbox(in_rast), "+init=epsg:32651"))



  # these was skipped since a common reproj function will be done (for now, output is always a sprawlext)

  # # All Ok - `sp` Extent object
  # testthat::expect_is(
  #   reproj_extent(raster::in_sprawlext_1
  #                                   "+init=epsg:3035", "+init=epsg:4326"), "Extent")  #nolint
  #
  # # All Ok - `sf` bbox
  # in_pts   <- read_vect(system.file("extdata/shapes","randpoints.shp",
  #                                   package = "sprawl.data"))
  # testthat::expect_is(reproj_extent(sf::st_bbox(in_pts),
  #                                   "+init=epsg:3035", "+init=epsg:4326"), "bbox")  #nolint
  #
  # # no proj set
  # testthat::expect_error(reproj_extent(in_pts, "+init=epsg:3035", "+init=epsg:4326"))  #nolint


})
