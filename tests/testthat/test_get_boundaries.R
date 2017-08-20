context("Download polygon boundaries")

test_that("get_boundaries works as expected", {

  expect_is(get_boundaries("CH", level = 0), "sf")

})
