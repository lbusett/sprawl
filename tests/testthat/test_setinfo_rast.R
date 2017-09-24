context("Add info attribute to a raster object")

test_that("setinfo_rast works as expected", {
  # library(testthat)
  skip_on_travis()
  #' If passing a RasterStack, we get correct info
  getinfo_obj <- raster::brick(system.file("extdata/OLI_test", "oli_multi_1000.tif",  #nolint
                                           package = "sprawl.data"))
  obj_new <- setinfo_rast(getinfo_obj)
  expect_is(obj_new@info, "list")
  expect_equal(obj_new@info$nbands, 6)

  #' If passing a subset, the correct band names are set in "indbands"
  obj_new <- setinfo_rast(getinfo_obj[[3]])
  expect_equal(obj_new@info$indbands, 3)

  #' If passing anything else, we get error
  getinfo_obj <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
                             package = "sprawl.data")
  expect_error(obj_new <- setinfo_rast(getinfo_obj))
  expect_error(obj_new <- setinfo_rast("aaa"))
  expect_error(obj_new <- setinfo_rast(10))

})
