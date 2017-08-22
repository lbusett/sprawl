context("Checking extraction of projection string")
testthat::test_that("Test projection string",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  testthat::expect_error(get_proj4string(in_pts))

  # extraction of proj4string from raster ----
  in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
  expect_equal(get_proj4string(in_rast),
               "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  expect_equal(get_proj4string(raster::raster(in_rast)),
               "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  # extraction of proj4string from vector ----
  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")

  expect_equal(get_proj4string(in_vect),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  expect_equal(get_proj4string(read_vect(in_vect)),
               "+proj=longlat +datum=WGS84 +no_defs")

  expect_equal(get_proj4string(read_vect(in_vect, as_sp = TRUE)),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


  # warning/abort on wrong proj4string ----
  rastobj <- raster::raster(in_rast)
  expect_warning(raster::crs(rastobj) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGSìèàrwa4")

  expect_warning(out <- get_proj4string(rastobj))
  expect_equal(out, "invalid")
  expect_error(get_proj4string(rastobj, abort = TRUE))

  # warning/abort on invalid filename or object ----
  expect_warning(out <- get_proj4string("pippo.shp"))
  expect_equal(out, "none")
  expect_error(get_proj4string(123, abort = TRUE))

}
)
