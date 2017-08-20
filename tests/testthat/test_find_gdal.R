context("Find GDAL path")

test_that("find_gdal works as expected", {
expect_is(find_gdal(), "character")

})
