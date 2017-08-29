context("Crop vector on a given extent")
testthat::test_that("Crop Vector object", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  library(raster)
  in_polys_file  <- system.file("extdata/shapes", "lc_polys.shp",
                                          package = "sprawl.data")
  in_rast_file   <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                                               package = "sprawl.data")
  cropped_vect   <- crop_vect(in_polys_file, in_rast_file)

  in_polys <- read_vect(in_polys_file)
  cropped_vect2   <- crop_vect(in_polys, in_rast_file)

  expect_equal(cropped_vect[["geometry"]], cropped_vect2[["geometry"]])
  expect_is(cropped_vect, "sf")

  in_rast     <- raster::raster(in_rast_file)
  out_feature <- in_polys[13,]
  crop_nointersect <- crop_vect(out_feature, in_rast)
  expect_equal(dim(crop_nointersect)[1], 0)
})
