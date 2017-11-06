context("select_extent")

test_that("select_extent works as expected", {
  # library(testthat)
  skip_on_travis()
  skip_on_cran()
  if (interactive()) {
    extent    <- select_extent()
    expect_is(extent, "sprawlext")
  }
})

