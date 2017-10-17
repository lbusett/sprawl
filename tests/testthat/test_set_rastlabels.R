context("set_rastlabels")


test_that("set_rastlabels works as expected", {
  library(raster)
  library(magrittr)
    in_rast <- raster::raster(ncol = 5, nrow = 5) %>%
       init("row")
    cat_rast <- set_rastlabels(in_rast)
    expect_is(cat_rast@data@attributes[[1]], "data.frame")
    expect_is(cat_rast, "Raster")
})
