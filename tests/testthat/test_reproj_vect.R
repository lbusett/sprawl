in_vect <- system.file("extdata/shapes","lc_polys.shp",
                         package = "sprawl.data")
r <- system.file("extdata/", "OLI_test/oli_multi_1000.tif",
                 package = "sprawl.data")
shp <- system.file("extdata/", "shapes/oli_polys.shp",
                   package = "sprawl.data")

# reproj_vect on spatial files ####
test_that("reproj_vect on spatial files", {
  context("reproject using spatial files as reference")
  out <- reproj_vect(ext, r, verbose = FALSE)
  expect_equal(get_proj4string(out), get_proj4string(r))
  out <- reproj_vect(ext, shp, verbose = FALSE)
  expect_equal(get_proj4string(out), get_proj4string(shp))
})

# reproj_vect on R objects ####
test_that("reproj_vect on R objects", {
  context("reproject using R objects as reference")
  vec <- read_vect(in_vect)
  r   <- read_rast(r)
  shp <- read_vect(shp)
  out <- reproj_vect(vec, r, verbose = FALSE)
  expect_equal(get_proj4string(out), get_proj4string(r))
  out <- reproj_vect(vec, shp, verbose = FALSE)
  expect_equal(get_proj4string(out), get_proj4string(shp))
})

# casting of reproj_vect output ####
test_that("casting of reproj_vect output", {
  context("reproject to a specific vector class")
  # ext <- read_vect(in_vect)
  out <- reproj_vect(vec, r, out_class = "sp", verbose = FALSE)
  expect_is(out, "Spatial")
  out <- reproj_vect(vec, shp, verbose = FALSE)
  expect_is(out, "sf")
  ext <- as(read_vect(in_vect), "Spatial")
  out <- reproj_vect(ext, r, verbose = FALSE)
  expect_is(out, "Spatial")

  out <- reproj_vect(vec, r, out_class = "sf", verbose = FALSE)
  expect_is(out, "sf")
})

# save reproj_vect output to file ####
test_that("save reproj_vect output to file", {
  context("save reproj_vect output to file")
  out_file <- tempfile(fileext = ".shp")
  out <- reproj_vect(vec, r, out_file = out_file, verbose = FALSE)
  expect_is(out, "sf")
  out_file <- tempfile(fileext = ".shp")
  out <- reproj_vect(ext, r, out_file = out_file, verbose = FALSE,
                     out_type = "vectfile")
  expect_true(file.exists(out))

})
