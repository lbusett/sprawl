test_that("reclass_rast/categorize_rast works as expected", {

  context("categorize/reclassify a raster")
  library(raster)
  library(magrittr)
  # create a test raster
  in_rast <- raster::raster(ncol = 20, nrow = 20) %>%
    raster::init("row") %>%
    set_rastlabels(class_names = letters[1:20])

  reclass_matrix <- tibble::tribble(
    ~start, ~end, ~new, ~label,
    0,   5,    1, "pippo",
    5,   8,    2, "pluto",
    8,   12,   2, "pluto",
    12,  15,   NA, NA,
    15,  20,   3, "paperino")
  # Vanilla class names
  expect_warning(out_rast <- recategorize_rast(in_rast,
                                               reclass_matrix,
                                               verbose = FALSE))
  expect_is(out_rast, "Raster")
  expect_equal(cellStats(out_rast, max, na.rm = TRUE), 3)
  expect_equal(cellStats(out_rast, min, na.rm = TRUE), 1)

  context("assign also colors")
  reclass_matrix <- tibble::tribble(
    ~start, ~end, ~new, ~label, ~color,
    0,   5,    1, "pippo", "red",
    5,   8,    2, "pluto", "green",
    8,   12,   2, "pluto", "green",
    12,  15,   NA, NA,     NA,
    15,  20,   3, "paperino", "purple")
  out_rast <- recategorize_rast(in_rast, reclass_matrix, verbose = FALSE)

})
