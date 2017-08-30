context("check_proj4string")

testthat::test_that("check_proj4string works as expected", {
  skip_on_travis()
  library(sp)

  # invalid proj4
  expect_error(check_proj4string("+init=epsg:montemario", abort = TRUE))
  expect_warning(check_proj4string("+init=epsg:montemario"))

  # valid proj4
  expect_equal(
    check_proj4string(sp::CRS("+init=epsg:32632")),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # same using CRS
  expect_equal(
    check_proj4string("+init=epsg:32632"),
    "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  in_rast <- read_rast(in_rast)
  expect_is(check_proj4string(in_rast@crs), "character")

})
