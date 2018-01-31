context("change_fileext")


test_that("change_fileext works as expected", {
  in_file <- "home/lb/tmp/prova.tif"
  testthat::expect_equal(change_fileext(in_file, ".png"), "home/lb/tmp/prova.png")
  testthat::expect_equal(change_fileext(in_file, ".png",
                                        new_path = "/home/lb/newpath"),
                         "/home/lb/newpath/prova.png")
  testthat::expect_equal(change_fileext(in_file, ".png", new_path = ""), "prova.png")
})
