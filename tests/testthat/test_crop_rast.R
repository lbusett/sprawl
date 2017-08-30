context("Crop raster on a given extent")
testthat::test_that("Test On raster cropping", {
  # skip_on_cran()
  skip_on_travis()
  library(testthat)
  library(sprawl.data)
  library(raster)
  in_file <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
                                       package = "sprawl.data")
  in_rast <- read_rast(in_file)
  in_vect <- create_fishnet(in_rast, pix_for_cell = 60)[50,]
  out_cropped  <- crop_rast(in_rast, in_vect, verbose = FALSE,
                            out_type = "rastobject")
  out_vrt      <- crop_rast(in_rast, in_vect, verbose = FALSE,
                            out_type = "vrtfile")
  out_rastfile <- crop_rast(in_rast, in_vect, verbose = FALSE,
                            out_type = "rastfile")
  expect_is(out_cropped, "Raster")
  expect_is(out_vrt, "character")
  expect_is(out_rastfile, "character")
  expect_is(raster::brick(out_vrt), "Raster")
  expect_is(raster::brick(out_rastfile), "Raster")
  expect_equal(as.numeric(raster::getValues(raster::brick(out_vrt))),
               as.numeric(raster::getValues(raster::brick(out_rastfile))))
  expect_error(expect_warning(crop_rast("ciao", in_vect)))
  expect_error(expect_warning(crop_rast(in_rast, "ciao.shp")))


  in_rast <- raster::stack(c(system.file("extdata/OLI_test", "oli_multi_1000_b1.tif", #nolint
                                         package = "sprawl.data")),
                           system.file("extdata/OLI_test", "oli_multi_1000_b2.tif", #nolint
                                       package = "sprawl.data"))
  out_cropped  <- crop_rast(in_rast, in_vect, verbose = FALSE,
                            out_type = "rastobject")
  expect_is(out_cropped, "Raster")
  expect_equal(raster::nlayers(out_cropped), 2)

})
