context("categorize_rast")


test_that("categorize_rast works as expected", {
  library(raster)
  library(magrittr)
    in_rast <- raster::raster(ncol = 5, nrow = 5) %>%
       init("row")
    cat_rast <- categorize_rast(in_rast)
    expect_is(levels(cat_rast)[[1]], "data.frame")
    expect_is(cat_rast, "Raster")
})
