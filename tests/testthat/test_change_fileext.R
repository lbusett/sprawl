context("change_fileext")


test_that("change_fileext works as expected", {
  in_file = "home/lb/tmp/prova.tif"
  testthat::expect_is(change_fileext(in_file, ".png"), "character")

  testthat::expect_is(change_fileext(in_file, ".png", full_path = T), "character")
})
