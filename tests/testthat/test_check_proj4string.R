context("check_proj4string tests")

testthat::test_that("check_proj4string works as expected", {
  # skip_on_travis()
  library(sp)

  # valid character
  expect_equal(
    check_proj4string("32N"),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

    expect_equal(
    check_proj4string("32S"),
    "+init=epsg:32732 +proj=utm +zone=32 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint


  expect_equal(
    check_proj4string(sp::CRS("+init=epsg:32632")),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  # valid numeric

  expect_equal(
    check_proj4string(32),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  expect_equal(
    check_proj4string(3857),
    "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs") #nolint


  # same using CRS
  expect_equal(
    check_proj4string("+init=epsg:32632"),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  in_rast <- read_rast(in_rast)
  expect_equal(check_proj4string(in_rast@crs),
               "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") #nolint

  # invalid inputs
  expect_error(check_proj4string("+init=epsg:montemario", abort = TRUE))
  expect_error(check_proj4string("123554644", abort = TRUE))
  expect_error(check_proj4string(123554644, abort = TRUE))
  expect_warning(check_proj4string("+init=epsg:montemario"))


})
