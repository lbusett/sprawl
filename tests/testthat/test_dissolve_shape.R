context("dissolve_shape")


test_that("dissolve_shape works as expected", {
  library(sprawl.data)
  indata    <- read_vect(system.file("extdata/shapes","lc_polys.shp", package = "sprawl.data"))
  byvar     <- "category"
  out_shape <- dissolve_shape(indata, byvar)

})
