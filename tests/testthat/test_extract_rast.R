context("Extract data from raster - on polygons")
testthat::test_that("Test On polygons extraction", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  library(testthat)
  in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
                                          package = "sprawl.data"),
                              stringsAsFactors = T)
  in_file  <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                                package = "sprawl.data")
  in_rast  <- read_rast(in_file)
  in_rast  <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8),
                                                    year = 2013))

  # check errors in input selbands
  expect_error(extract_rast(in_rast, in_polys, selbands = c(3,1)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c(3,NA)))
  expect_error(extract_rast(in_rast, in_polys, selbands = 1))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-01",3)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-20","2013-01-08"))) #nolint

  out <- extract_rast(in_rast, in_polys, selbands = c("2013-01-01","2013-01-08"),  #nolint
                      verbose = FALSE)
  expect_is(out, "list")
  out <- extract_rast(in_rast, in_polys, selbands = c(2,3), verbose = FALSE)
  expect_is(out, "list")
  # Check that chunked and non-chunked processing yields the same results
  out  <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                       selbands = c(1,2), small = F)
  out2 <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                       selbands = c(1,2), maxchunk = 30000,  small = F)
  expect_equal(out$alldata$value, out2$alldata$value)
  expect_equal(out$stats$avg, out2$stats$avg)

  # Check that processing with and without valid id_field are identical
  out    <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                         selbands = c(1,2), small = T, id_field = "id")
  out2   <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                         selbands = c(1,2), small = T, id_field = "lc_type",
                         addfeat = FALSE)

  out3  <- expect_warning(extract_rast(in_rast, in_polys, verbose = F,
                                       keep_null = T, selbands = c(1,2),
                                       small = T, id_field = "lc_tydfse"))
  out4  <- extract_rast(in_rast, in_polys, verbose = F,
                        keep_null = T, selbands = c(1,2), small = T)
  expect_equal(out$stats$avg, out2$stats$avg)
  expect_equal(out2$stats$sd, out4$stats$sd)
  expect_equal(out$alldata$value, out2$alldata$value)
  expect_equal(out$alldata$value, out4$alldata$value)

  # Check that processing with and without comp_quant are equal for a common
  # variable
  out2  <- extract_rast(in_rast, in_polys, verbose = T, keep_null = T,
                        selbands = c(1,2), small = T, comp_quant = TRUE)
  expect_equal(out$stats$avg, out2$stats$avg)
  expect_equal(out$stats$sd, out2$stats$sd)

  # Check that results are coherent with `raster::extract` on the test dataset
  out_extract_rast  <- extract_rast(in_rast, in_polys, selbands = c(1,2),
                                    verbose = F, keep_null = T, addgeom = F,
                                    full_data = F, small = T,
                                    comp_quant = FALSE)
  outcomp <- out_extract_rast$stats$avg
  expect_warning(out_extract <- raster::extract(in_rast[[1:2]],
                                                as(in_polys, "Spatial"),
                                                fun = "mean", na.rm = T))
  expect_equal(mean(as.numeric(out_extract), na.rm = TRUE),
               mean(outcomp, na.rm = TRUE))
  outcustom <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = F,
                            keep_null = T, addgeom = F, full_data = F, small = T, #nolint
                            comp_quant = FALSE, FUN = mean)
  outcomp <- outcustom$stats$myfun
  expect_equal(mean(as.numeric(out_extract), na.rm = TRUE),
               mean(outcustom$stats$myfun, na.rm = TRUE))
})

context("Extract data from raster - on points")
testthat::test_that("Test On points extraction", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  library(testthat)
  in_pts   <- read_vect(system.file("extdata/shapes","randpoints.shp",
                                    package = "sprawl.data"))
  in_file  <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                          package = "sprawl.data")
  in_rast  <- read_rast(in_file)
  in_rast  <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8), year = 2013))
  out      <- extract_rast(in_rast[[1:3]], in_pts, id_field = "id",
                           verbose = FALSE, keep_null = T)  %>%
                   tibble::as_tibble()

  # Output is a data frame
  testthat::expect_is(out, "data.frame")
  out_extract <- raster::extract(in_rast[[1:3]], as(in_pts, "Spatial")) %>%
    tibble::as_tibble()
  # Output is equal to raster::extract
  testthat::expect_equal(as.numeric(t(as.matrix(out[,3:103]))),
                         as.numeric(as.matrix(out_extract)))

  out <- extract_rast(in_rast[[1:3]], in_pts, id_field = "id",
                      verbose = F, keep_null = T, long_format = T)
  # On `long = TRUE` output is `sf`
  testthat::expect_is(out, "sf")

  # On `long = TRUE` but add_geom = FALSE output is not a `sf`
  out <- extract_rast(in_rast[[1:3]], in_pts, id_field = "id",
                      verbose = F, addgeom = F)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_error(st_geometry(out))
})

