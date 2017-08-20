context("Find GDAL path")

test_that("find_gdal works as expected", {
  skip_on_travis()
expect_is(find_gdal(), "character")

})
