context("Zonal Statistics - polygons")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  skip_on_travis()

library(raster)
  in_rast_values = raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl"))[[20]]
  in_obj_zones   = in_rast_values %>%
    raster::aggregate(fact = 4, filename = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\testaggr2.tif",
                      overwrite = T)


  raster::writeRaster(in_obj_zones, file = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\sprawl_EVItest_agg.tif",
                      overwrite = T)
  test = aggregate_rast(in_rast_values,
                 in_obj_zones,
                 FUN = mean,
                 method = "fastdisk",
                 to_file = TRUE,
                 out_file = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\testaggr.tif",
                 verbose = FALSE)
})

profvis::profvis({test = aggregate_rast(in_rast_values,
                                        in_obj_zones,
                                        FUN = mean,
                                        method = "fastdisk",
                                        to_file = TRUE,
                                        out_file = "D:\\Documents\\Source\\git\\sprawl\\inst\\extdata\\testaggr.tif",
                                        verbose = FALSE)}, torture = 1, interval = 0.005)
