context("sprawlext-class")


test_that("sprawlext-class works as expected", {

  # skip_on_travis()
  library(raster)
  library(sp)

  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  ex_sprawlext <- get_extent(in_rast)

  # Check format
  expect_is(ex_sprawlext, "sprawlext")
  expect_is(ex_sprawlext@proj4string, "character")
  in_crs <- raster(in_rast)@crs
  expect_equal(ex_sprawlext@proj4string, in_crs@projargs)
  expect_is(ex_sprawlext@extent, "numeric")
  expect_equal(length(ex_sprawlext@extent), 4)
  expect_equal(names(ex_sprawlext@extent), c("xmin","ymin","xmax","ymax"))

  # Convert sprawlext to Extent
  expect_is(as(ex_sprawlext, "Extent"), "Extent")

  # Convert to bbox
  expect_is(as(ex_sprawlext, "matrix"), "matrix")

  # Convert to Spatial* objects
  expect_is(as(ex_sprawlext, "SpatialPoints"), "SpatialPoints")
  expect_is(as(ex_sprawlext, "SpatialLines"), "SpatialLines")
  expect_is(as(ex_sprawlext, "SpatialPolygons"), "SpatialPolygons")

  # Convert to sf objects
  expect_is(as(ex_sprawlext, "sfc_POLYGON"), "sfc_POLYGON")
  expect_is(as(ex_sprawlext, "sfc_POINT"), "sfc_POINT")

  # Extract proj4string CRS
  expect_is(as(ex_sprawlext, "CRS"), "CRS")

})
