context("Zonal Statistics - polygons")
testthat::test_that("Test On raster aggregation", {
  # skip_on_cran()
  skip_on_travis()


  in_rast_values = raster::stack(system.file("extdata", "sprawl_EVItest.tif", package = "sprawl"))[[20]]
  in_obj_zones   = in_rast_values %>%
    raster::aggregate(fact = 4)
  test = aggregate_rast(in_rast_values,
                 in_obj_zones,
                 FUN = mean,
                 method = "fastdisk",
                 to_file = TRUE,
                 out_file = "/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE_500_new.tif",
                 verbose = TRUE)
})
