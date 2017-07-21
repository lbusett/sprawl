context("Zonal Statistics - polygons")
testthat::test_that("Test On polygons extraction", {
  skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  in_polys       <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl.data"), stringsAsFactors = T)
  in_rast        <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl.data"))
  in_rast        <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8), year = 2013))

  # check errors in input selbands
  expect_error(extract_rast(in_rast, in_polys, selbands = c(3,1)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c(3,NA)))
  expect_error(extract_rast(in_rast, in_polys, selbands = 1))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-01",3)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-20","2013-01-08")))

  out <- extract_rast(in_rast, in_polys, selbands = c("2013-01-01","2013-01-08"), verbose = FALSE, mode = "std")
  expect_is(out, "list")
  out <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = FALSE, mode = "std")
  expect_is(out, "list")
  # Check that chunked and non-chunked processing yields the same results
  out  <- extract_rast(in_rast, in_polys, verbose = F, long = F, keep_null = T, selbands = c(1,2), small = F)
  out2 <- extract_rast(in_rast, in_polys, verbose = F, long = F, keep_null = T, selbands = c(1,2), maxchunk = 30000,  small = F)
  expect_equal(dplyr::select(out$alldata, -geometry), dplyr::select(out2$alldata, -geometry))
  expect_equal(dplyr::select(out$stats, -geometry), dplyr::select(out2$stats, -geometry))

  # Check that processing with and without valid id_field are identical
  out    <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "id")
  out2   <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "lc_type",
                         addfeat = FALSE)
  # out2  <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "lc_type")
  out3  <- expect_warning(extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "lc_tydfse"))
  out4  <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T)
  expect_equal(out$stats$value, out2$stats$value)
  expect_equal(out2$stats$value, out4$stats$value)
  expect_equal(out$alldata$value, out2$alldata$value)
  expect_equal(out$alldata$value, out4$alldata$value)
  # Check that processing with and without comp_quant are equal for a common variable
  # out   <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "id", comp_quant = TRUE)
  out2  <- extract_rast(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, comp_quant = TRUE)
  expect_equal(dplyr::filter(out$stats, variable == "avg")$value, dplyr::filter(out2$stats, variable == "avg")$value)
  expect_equal(dplyr::filter(out$stats, variable == "sd")$value, dplyr::filter(out2$stats, variable == "sd")$value)

  # Check that results are coherent with `raster::extract` on the test dataset
  out_extract_rast  <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = F, long = F, keep_null = T, addgeom = T, full_data = F, small = T,  comp_quant = FALSE)
  outcomp = out_extract_rast$stats$avg
  expect_warning(out_extract <- raster::extract(in_rast[[1:2]], as(in_polys, "Spatial"), fun = "mean", na.rm = T))
  expect_equal(mean(as.numeric(out_extract), na.rm = TRUE), mean(outcomp, na.rm = TRUE))
  outcustom <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = F, long = F, keep_null = T,
                          addgeom = F, full_data = F, small = T,  comp_quant = FALSE, FUN = mean)
  outcomp = outcustom$stats$myfun
  expect_equal(mean(as.numeric(out_extract), na.rm = TRUE), mean(outcustom$stats$myfun, na.rm = TRUE))
})
