test_that("reproj_rast works as expected", {

  skip_on_travis()
  library(raster)
  in_file <- system.file("extdata/OLI_test", "oli_multi_1000_b2.tif",
                         package = "sprawl.data")
  context("reproject on a proj4string, using a filename or a raster object")
  out_proj <- "+init=epsg:3035"
  out1 <- expect_is(reproj_rast(in_file, out_proj, out_filename =
                                  tempfile(fileext = ".tif")), "Raster")
  out2 <- reproj_rast(read_rast(in_file), out_proj, out_filename =
                        tempfile(fileext = ".tif"))
  expect_equal(getValues(out1), getValues(out2))
  expect_equal(
    get_proj4string(out2),
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") #nolint

  out3 <- expect_is(reproj_rast(in_file, "+init=epsg:4326", out_filename =
                                  tempfile(fileext = ".tif")), "Raster")
  expect_equal(get_proj4string(out3),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #nolint

  context("reproject on a proj derived from a spatial object, with or without cropping") #nolint

  in_vect <- read_vect(system.file("extdata/shapes","lc_polys.shp",
                                   package = "sprawl.data"))
  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")

  out4   <- reproj_rast(read_rast(in_rast), in_vect)
  out5   <- reproj_rast(read_rast(in_rast), in_vect[6,1], crop = T)

})
