context("Get information on a raster object/file")

test_that("get_rastinfo works as expected", {
  # skip_on_travis()
  library(sprawl.data)
  # Calling on a raster file or on a raster object accessing it gets same
  # results
  getinfo_obj <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
                             package = "sprawl.data")
  info_rastfile <- get_rastinfo(getinfo_obj)

  getinfo_obj <- raster::brick(system.file("extdata/OLI_test", "oli_multi_1000.tif",  #nolint
                                           package = "sprawl.data"))
  info_brick <- get_rastinfo(getinfo_obj)

  expect_equal(info_rastfile, info_brick)

  # Calling on a stack composed by bands coming from different files gets the
  # correct filenames
  getinfo_obj <- raster::stack(c(system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",  #nolint
                                             package = "sprawl.data")),
                               system.file("extdata/OLI_test", "oli_multi_1000_b2.tif",  #nolint
                                           package = "sprawl.data"))
  info_raststack <- get_rastinfo(getinfo_obj)
  expect_equal(length(unique(info_raststack$fnames)), 2)

  # Calling on a subset of a `Raster`object gets correct indbands values

  getinfo_obj <- raster::brick(system.file("extdata/OLI_test", "oli_multi_1000.tif",  #nolint
                                           package = "sprawl.data"))
  getinfo_obj <- getinfo_obj[[2]]
  info <- get_rastinfo(getinfo_obj)
  expect_equal(info$indbands, 2)

  # Calling on a raster without projection does not abort
  r = raster::raster(ncol = 10, nrow = 20)
  raster::crs(r) <- NA
  info <- get_rastinfo(r)


})
