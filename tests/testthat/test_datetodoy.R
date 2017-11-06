context("Date to DOY conversions")
testthat::test_that("Test datesconversions", {
  # skip_on_cran()
  # skip_on_travis()
  # convert date to doy OK
  expect_equal(datetodoy("2001-04-01"), 91)
  expect_equal(datetodoy("2004/04/01"), 92)
  expect_equal(datetodoy(as.Date("2000-04-01")), 92)
  expect_equal(datetodoy(c("2000-04-01", "2000-04-03")), c(92,94))

  # work with NAs
  expect_warning(out <- datetodoy(c("2000-04-01", NA)))
  expect_equal(out, c(92,NA))

  expect_error(out <- datetodoy(c("2000-04-01", NA), abort = TRUE))
  # convert date to doy incorrect parameters
  expect_error(datetodoy("2000-04-0d"))

})

