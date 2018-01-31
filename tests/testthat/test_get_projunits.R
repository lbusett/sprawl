context("get_projunits")

test_that("get_projunits works as expected", {
# skip_on_travis()
  # latlong projection
  proj_string <- "+init=epsg:4326"
  expect_equal(get_projunits(proj_string), "dec.degrees")

  # metric projection
  proj_string <- "+init=epsg:3857"
  expect_equal(get_projunits(proj_string), "m")

  # invalid projection
  proj_string <- "+init=epsg:38"
  expect_equal(expect_warning(get_projunits(proj_string)), NA)

  # input is CRS
  proj_string <- sp::CRS("+init=epsg:3857")
  expect_equal(get_projunits(proj_string), "m")

})
