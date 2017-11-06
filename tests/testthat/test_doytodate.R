context("DOY to date conversion")
test_that("doytodate works as expected", {

    # skip_on_cran()
    # skip_on_travis()
    # convert date to doy OK
    expect_equal(doytodate(100, 2015), as.Date("2015-04-10"))
    expect_equal(doytodate(100, 2016), as.Date("2016-04-09"))
    expect_equal(doytodate(c(100,102), 2015), as.Date(c("2015-04-10",
                                                        "2015-04-12")))

    # convert date to doy incorrect parameters
    expect_error(doytodate(100, -1))
    expect_error(doytodate(100, "3"))
    expect_error(doytodate(100, c(2000,2001)))

    # convert date to doy outside 0 365
    expect_message(doytodate(400, 2001, verbose = TRUE))

})
