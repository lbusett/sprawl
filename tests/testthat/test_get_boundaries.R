context("Download polygon boundaries")

test_that("get_boundaries works as expected", {
  # skip_on_travis()
  expect_is(get_boundaries("CH", level = 0), "sf")

})
