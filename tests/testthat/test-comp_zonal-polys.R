context("Zonal Statistics - polygons")
testthat::test_that("Test On polygons extraction", {
  # skip_on_cran()
  skip_on_travis()
  in_polys <- readshape(system.file("extdata","lc_polys.shp", package = "sprawl"), stringsAsFactors = T)
  in_rast  <- raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl"))

  # check errors in input selbands
  expect_error(comp_zonal(in_rast, in_polys, selbands = c(3,1)))
  expect_error(comp_zonal(in_rast, in_polys, selbands = c(3,NA)))
  expect_error(comp_zonal(in_rast, in_polys, selbands = 1))
  expect_error(comp_zonal(in_rast, in_polys, selbands = c("2013-01-01",3)))
  expect_error(comp_zonal(in_rast, in_polys, selbands = c("2013-01-20","2013-01-08")))

  expect_error(out <- comp_zonal(in_rast, in_polys, selbands = c("2013-01-01","2013-01-08"), verbose = FALSE, mode = "std", maxchunk = 10E4))
  expect_is(out, "list")
  expect_warning(out <- comp_zonal(in_rast, in_polys, selbands = c(1,2), verbose = FALSE, mode = "std", maxchunk = 10E4))
  expect_is(out, "list")
  # Check that chunked and non-chunked processing yields the same results
  expect_warning(out  <- comp_zonal(in_rast, in_polys, verbose = F, long = F, keep_null = T, selbands = c(1,2), small = F))
  expect_warning(out2 <- comp_zonal(in_rast, in_polys, verbose = F, long = F, keep_null = T, selbands = c(1,2), maxchunk = 10000,  small = F))
  expect_equal(dplyr::select(out$alldata, -geometry), dplyr::select(out2$alldata, -geometry))
  expect_equal(dplyr::select(out$stats, -geometry), dplyr::select(out2$stats, -geometry))

  # Check that processing with and without id_field are identical
  expect_warning(out   <- comp_zonal(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "id"))
  expect_warning(out2  <- comp_zonal(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "lc_type"))
  expect_warning(out3  <- comp_zonal(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T))
  expect_equal(out$stats$value, out2$stats$value, out3$stats$value)
  expect_equal(out$alldata$value, out2$alldata$value, out3$alldata$value)

  # Check that processing with and without comp_quant are equal for a common variable
  expect_warning(out  <- comp_zonal(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, id_field = "id", comp_quant = TRUE))
  expect_warning(out2  <- comp_zonal(in_rast, in_polys, verbose = F, long = T, keep_null = T, selbands = c(1,2), small = T, comp_quant = TRUE))
  expect_equal(dplyr::filter(out$stats, variable == "avg")$value, dplyr::filter(out2$stats, variable == "avg")$value)
  expect_equal(dplyr::filter(out$stats, variable == "sd")$value, dplyr::filter(out2$stats, variable == "sd")$value)

  # Check that results are coherent with `raster::extract` on the test dataset
  expect_warning(out_comp_zonal  <- comp_zonal(in_rast, in_polys, selbands = c(1,2), verbose = F, long = F, keep_null = T, addgeom = F, full_data = F, small = T,  comp_quant = FALSE))
  outcomp = out_comp_zonal$stats$avg
  expect_warning(out_extract <- raster::extract(in_rast[[1:2]], as(in_polys, "Spatial"), fun = "mean", na.rm = T))
  expect_equal(mean(as.numeric(out_extract), na.rm = TRUE), mean(outcomp, na.rm = TRUE))
})


qgis
