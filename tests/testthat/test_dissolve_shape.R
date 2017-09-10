context("dissolve_shape")


test_that("dissolve_shape works as expected", {
  library(sprawl.data)
  indata    <- read_vect(system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data"))
  byvar     <- "category"
   expect_is(dissolve_shape(indata, byvar), "sf")

})
