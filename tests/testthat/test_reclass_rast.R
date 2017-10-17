context("categorize/reclassify a raster")


test_that("reclass_rast/categorize_rast works as expected", {

  library(raster)
  library(magrittr)
  # create a test raster
  in_rast <- raster::raster(ncol = 20, nrow = 20) %>%
    raster::init("row") %>%
    set_rastlabels(class_names = letters[1:20])

  plot_rast_gg(in_rast, rast_type = "continuous")

  reclass_matrix <- tibble::tribble(
    ~start, ~end, ~new,
    0,   5,    1,
    5,   8,    2,
    8,   12,   2,
    12,  15,   NA,
    15,  20,   3)
  # Vanilla class names
  out_rast <- reclass_rast(in_rast, reclass_matrix, verbose = FALSE)
  expect_is(out_rast, "Raster")
  expect_equal(cellStats(out_rast, max, na.rm = TRUE), 3)
  expect_equal(cellStats(out_rast, min, na.rm = TRUE), 1)

  # Assign class names
  out_rast <- categorize_rast(in_rast, reclass_matrix, verbose = FALSE,
                              c("A", "B", "C"))
  expect_is(levels(out_rast)[[1]], "data.frame")

  # Too few class names --> Warning
  expect_warning(out_rast <- reclass_rast(in_rast, reclass_matrix,
                                          c("A", "B"),
                                          verbose = FALSE))


})
