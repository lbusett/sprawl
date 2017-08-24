context("Get extent of a spatial object/file")
testthat::test_that("Test retrieval of extent",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)

  # extraction of proj4string from raster ----
  in_rast_file <- system.file("extdata/MODIS_test",
                              "EVIts_test.tif", package = "sprawl.data")
  in_rast      <- raster::raster(in_rast_file)

  ext_file     <- get_extent(in_rast_file)
  expect_is(ext_file, "sprawlext")
  ext_rast     <- get_extent(in_rast)
  expect_is(ext_rast, "sprawlext")
  expect_equal(ext_rast, ext_file)

  in_vect_file <- system.file("extdata/shapes","lc_polys.shp",
                              package = "sprawl.data")
  in_vect      <- read_vect(in_vect_file)
  in_vect_sp   <- read_vect(in_vect_file, as_sp = TRUE)
  ext_file <- get_extent(in_vect)
  ext_sf <- get_extent(in_vect)
  ext_sp <- get_extent(in_vect_sp)

  expect_is(ext_file, "sprawlext")
  expect_equal(ext_file, ext_sf)
  expect_equal(ext_sf@extent, ext_sp@extent)


  # warning/abort on wrong proj4string ----
  wrong_file <- "/tttt/wrong.tif"
  expect_error(get_extent(wrong_file, abort = TRUE))
  expect_warning(get_extent(wrong_file, abort = FALSE))


  # creation of sprawlext from extent / bbox and projection ---
  in_bbox <- sp::bbox(in_rast)
  in_extent <- raster::extent(in_rast)
  in_proj4 <- in_rast@crs

  ext_from_bbox   <- get_extent(in_bbox, proj4string = in_proj4)
  ext_from_extent <- get_extent(in_extent, proj4string = in_proj4)
  expect_is(ext_from_bbox, "sprawlext")
  expect_equal(ext_from_bbox, ext_from_extent)
  expect_error(get_extent(in_bbox, abort = TRUE))
  expect_error(get_extent(in_extent, abort = FALSE))

})
