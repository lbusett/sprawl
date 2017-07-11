context("Zonal Statistics - points")
testthat::test_that("Test On points extraction", {
  # skip_on_cran()
  skip_on_travis()
  in_pts   <- readshape(system.file("extdata","randpoints.shp", package = "sprawl"))
  in_rast        <- get(load(system.file("extdata", "sprawl_EVItest.RData", package = "sprawl")))
  out <- comp_zonal(in_rast, in_pts, id_field = "id", verbose = FALSE, long = F, keep_null = T)  %>%
                   tibble::as_tibble()

  # Output is a data frame
  testthat::expect_is(out, "data.frame")
  out_extract = raster::extract(in_rast, as(in_pts, "Spatial")) %>%
    tibble::as_tibble()
  # Output is equal to raster::extract
  testthat::expect_equal(as.numeric(t(as.matrix(out[,3:103]))), as.numeric(as.matrix(out_extract)))

  out <- comp_zonal(in_rast, in_pts, id_field = "id", verbose = F, long = T, keep_null = T)
  # On `long = TRUE` output is `sf`
  testthat::expect_is(out, "sf")

  # On `long = TRUE` but add_geom = FALSE output is not a `sf`
  out <- comp_zonal(in_rast, in_pts, id_field = "id", verbose = F, long = T, addgeom = F)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_error(st_geometry(out))
})

