context("Checking reprojection of extent")
testthat::test_that("Test reproj_extent",{
  # skip_on_cran()
  skip_on_travis()
  # no proj set
  library(sprawl.data)
  testthat::expect_error(reproj_extent(in_pts))

  # extraction of projstring from raster ----
  in_rast <- system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data")
  expect_equal(get_projstring(in_rast),
               "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  expect_equal(get_projstring(raster::raster(in_rast)),
               "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  # extraction of projstring from vector ----
  in_vect <- system.file("extdata","lc_polys.shp", package = "sprawl.data")

  expect_equal(get_projstring(in_vect),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  expect_equal(get_projstring(read_vect(in_vect)),
               "+proj=longlat +datum=WGS84 +no_defs")

  expect_equal(get_projstring(read_vect(in_vect, as_sp = TRUE)),
               "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


  # abort on wrong projstring ----
  rastobj <- raster::raster(in_rast)
  raster::crs(rastobj) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGSìèàrwa4"

  expect_warning(get_projstring(rastobj))
  expect_error(get_projstring(rastobj, abort = TRUE))

  expect_error(get_projstring("pippo.shp"))
  expect_error(get_projstring(123))

}
)
