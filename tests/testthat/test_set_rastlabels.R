test_that("set_rastlabels works as expected", {
  context("set_rastlabels --> Assign auto labels and colors")

  require(raster)
  require(magrittr)

  in_rast <- raster::raster(ncol = 5, nrow = 5) %>%
    init("row")
  expect_warning(cat_rast <- set_rastlabels(in_rast, verbose = FALSE))
  expect_is(cat_rast@data@attributes[[1]], "data.frame")
  expect_is(cat_rast, "Raster")

  context("set_rastlabels --> Assign manual label names")

  cat_rast <- set_rastlabels(in_rast,
                             class_names = letters[1:5],
                             verbose = FALSE)
  expect_is(cat_rast, "Raster")
  levs <- raster::levels(cat_rast)[[1]]$Class
  expect_equal(levs, letters[1:5])

  # wrong labels
  expect_warning(cat_rast <- set_rastlabels(in_rast, class_names = letters[1:4],
                                            verbose = FALSE))


})
