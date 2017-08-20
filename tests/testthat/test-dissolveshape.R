context("Dissolve vector object")
testthat::test_that("Test dissolve shapefile",{
  library(testthat)
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  indata <- read_vect(system.file("extdata/shapes","lc_polys.shp",
                                  package = "sprawl.data"))
  byvar <- "category"

  # correct processing: no error and output is "sf" object
  expect_silent(out <- dissolve_shape(indata, byvar))
  # expect_false(is.null(st_geometry(dissolve_shape(indata, byvar))))
  expect_silent(out <- dissolve_shape(indata, "sup_catego"))


  # various errors
  expect_error(dissolve_shape(indata, "sup_co"))  # wrong grouing variable
  bad_in <- 100
  expect_error(dissolve_shape(bad_in, byvar))  # input is not a sf or sp object

  # dissolve on supercategory
  expect_equal(length(unique(out$id)),1)
  expect_gt(length(unique(out$sup_catego)),1)
})
