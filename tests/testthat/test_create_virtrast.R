test_that("create_virtrast works as expected on Raster Layers", {
  context("create_virtrast on raster Layer")
  library(sprawl.data)
  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  in_rast <- read_rast(in_file, bands = 5)
  vrt     <- read_rast(create_virtrast(in_rast))
  expect_is(vrt, "RasterLayer")
  expect_equal(summary(getValues(vrt)), summary(getValues(in_rast)))

  context("create_virtrast on Raster Brick and save to specific file") # ####

  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  in_rast <- read_rast(in_file)
  outfile <- tempfile(fileext = ".vrt")
  vrt <- read_rast(create_virtrast(in_rast, outfile))
  expect_is(vrt, "RasterBrick")
  expect_equal(summary(getValues(vrt[[10]])), summary(getValues(in_rast[[10]])))

  context("create_virtrast on Raster Stack ") # ####

  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  in_rast <- raster::stack(in_file)
  vrt <- read_rast(create_virtrast(in_rast))
  expect_is(vrt, "RasterBrick")
  expect_equal(summary(getValues(vrt[[10]])), summary(getValues(in_rast[[10]])))

  context("create_virtrast on Raster Stack with bands from different files") # #### #nolint

  in_rast1   <- system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",
                            package = "sprawl.data")
  in_rast2   <- system.file("extdata/OLI_test", "oli_multi_1000_b2.tif",
                            package = "sprawl.data")
  in_rast <- raster::stack(in_rast1, in_rast2)
  vrt <- read_rast(create_virtrast(in_rast))
  expect_is(vrt, "RasterBrick")
  expect_equal(getValues(vrt)[,2], getValues(in_rast)[,2])

  context("create_virtrast on Raster Stack derived by subsetting a brick") # ####

  in_rast <- read_rast(in_file)[[c(2,8)]]
  vrt <- read_rast(create_virtrast(in_rast))
  expect_is(vrt, "RasterBrick")
  expect_equal(getValues(vrt)[,1], getValues(in_rast)[,1])

  context("Fail on bad input") # ####

  in_file <- system.file("extdata/MODIS_test", "EVIts_tesddt.tif",
                         package = "sprawl.data")
  expect_error(create_virtrast(in_file))

  in_file <- system.file("extdata/shapes", "poly_lomb.RData",
                         package = "sprawl.data")
  expect_error(create_virtrast(in_file))

})
