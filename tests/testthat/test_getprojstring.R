context("Get projection string of a spatial object/file")
testthat::test_that("Test projection string",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  library(testthat)
  library(raster)

  # extraction of projstring from raster ----
  in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                         package = "sprawl.data")
  expect_equal(
    get_projstring(in_rast),
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"  #nolint
  )

  expect_equal(
    get_projstring(raster::raster(in_rast)),
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"  #nolint
  )

  # extraction of projstring from vector ----
  in_vect <- system.file("extdata/shapes","lc_polys.shp",
                         package = "sprawl.data")

  expect_equal(get_projstring(in_vect),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  #nolint

  expect_equal(get_projstring(read_vect(in_vect)),
               "+proj=longlat +datum=WGS84 +no_defs")

  expect_equal(get_projstring(read_vect(in_vect, as_sp = TRUE)),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  #nolint

  # warning/abort on wrong projstring ----
  rastobj <- raster::raster(in_rast)
  expect_warning(raster::crs(rastobj) <-
                   "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGSìèàrwa4")  #nolint

  expect_warning(out <- get_projstring(rastobj))
  expect_equal(out, "invalid")
  expect_error(get_projstring(rastobj, abort = TRUE))

  # warning/abort on invalid filename or object ----
  expect_warning(out <- get_projstring("pippo.shp"))
  expect_equal(out, "none")
  expect_error(get_projstring(123, abort = TRUE))

}
)
