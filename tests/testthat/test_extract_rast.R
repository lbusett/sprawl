context("Extract data from raster - on polygons") #----
skip_on_travis()
library(sprawl.data)
library(testthat)
library(sf)

# This builds a test raster and polygon shape with variable conditions:
# polygons inside, outside and partially inside the raster !

in_rast  <- build_testraster(100,100,2)
set.seed(1)
in_polys <- create_fishnet(in_rast, pix_for_cell = 5) %>%
  dplyr::sample_n(size = 10) %>%
  dplyr::mutate(id = seq(1,10),
                field_1 = sample(letters, 10),
                field_2 = sample(1:10, 10))
cut_ext <- c(-100, -85, 100, 81)
names(cut_ext) <- c("xmin", "ymin", "xmax", "ymax")
in_rast  <- raster::setZ(in_rast,
                         doytodate(seq(1,16, by = 8), year = 2013)) %>%
  crop_rast(methods::new("sprawlext",
                         extent = cut_ext, proj4string = "+init=epsg:4326"),
            verbose = FALSE)


# in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
#                                   package = "sprawl.data"),
#                       stringsAsFactors = T)
# in_file  <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#                         package = "sprawl.data")
# in_rast  <- read_rast(in_file)
# in_rast  <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8),
#                                             year = 2013))
testthat::test_that("Basic test on polygons extraction", {

  # skip_on_cran()
  skip_on_travis()

  # check errors in input selbands
  expect_error(extract_rast(in_rast, in_polys, selbands = c(2,1)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c(2,NA)))
  expect_error(extract_rast(in_rast, in_polys, selbands = 1))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-01",3)))
  expect_error(extract_rast(in_rast, in_polys, selbands = c("2013-01-20","2013-01-08"))) #nolint

  out <- extract_rast(in_rast, in_polys, selbands = c("2013-01-01","2013-01-08"),  #nolint
                      verbose = FALSE, join_feat_tbl = T)
  out2 <- extract_rast(in_rast, in_polys, selbands = c("2013-01-01","2013-01-08"),  #nolint
                       verbose = FALSE, join_feat_tbl = F, join_geom = F)
  expect_is(out, "list")
  expect_equal(out$stats$avg, out2$stats$avg)
  out <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = FALSE)
  expect_is(out, "list")
  # Check that chunked and non-chunked processing yields the same results
  out  <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                       selbands = c(1,2), small = F, join_geom = F)
  out2 <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                       selbands = c(1,2), maxchunk = 30000,  small = F)
  expect_equal(out$alldata$value, out2$alldata$value)
  expect_equal(out$stats$avg, out2$stats$avg)
})

testthat::test_that(
  "Polygons extraction with and without valid id_field are identical", {
    skip_on_travis()
    # Check that processing with and without valid id_field are identical
    out    <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                           selbands = c(1,2), small = T, id_field = "id")
    out2   <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                           selbands = c(1,2), small = T, id_field = "field_1",
                           join_feat_tbl = FALSE)

    out3  <- expect_warning(extract_rast(in_rast, in_polys, verbose = F,
                                         keep_null = T, selbands = c(1,2),
                                         small = T, id_field = "lc_tydfse"))
    out4  <- extract_rast(in_rast, in_polys, verbose = F,
                          keep_null = T, selbands = c(1,2), small = T)
    expect_equal(out$stats$avg, out2$stats$avg)
    expect_equal(out2$stats$sd, out4$stats$sd)
    expect_equal(out$alldata$value, out2$alldata$value)
    expect_equal(out$alldata$value, out4$alldata$value)
  })

testthat::test_that(
  "Polygons extraction with and without comp_quant are equal", {
    skip_on_travis()
    # Check that processing with and without comp_quant are equal for a common
    # variable
    out    <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                           selbands = c(1,2), small = T, id_field = "id")
    out2  <- extract_rast(in_rast, in_polys, verbose = F, keep_null = T,
                          selbands = c(1,2), small = T, comp_quant = TRUE)
    expect_equal(out$stats$avg, out2$stats$avg)
    expect_equal(out$stats$sd, out2$stats$sd)
  })

testthat::test_that(
  "Polygons extraction results are coherent with `raster::extract`", {
    # Check that results are coherent with `raster::extract` on the test dataset
    skip_on_travis()
    out_extract_rast  <- extract_rast(in_rast, in_polys, selbands = c(1,2),
                                      verbose = F, keep_null = T, join_geom = F,
                                      full_data = F, small = T,
                                      comp_quant = FALSE)
    outcomp <- out_extract_rast$stats$avg
    out_extract <- raster::extract(in_rast[[1:2]],
                                   as(in_polys, "Spatial"),
                                   fun = "mean", na.rm = T)
    expect_equal(mean(as.numeric(out_extract), na.rm = TRUE),
                 mean(outcomp, na.rm = TRUE))
    outcustom <- extract_rast(in_rast, in_polys, selbands = c(1,2), verbose = F,
                              keep_null = T, join_geom = F, full_data = F, small = T, #nolint
                              comp_quant = FALSE, FUN = mean)
    outcomp <- outcustom$stats$myfun
    expect_equal(mean(as.numeric(out_extract), na.rm = TRUE),
                 mean(outcustom$stats$myfun, na.rm = TRUE))
  })

context("Extract data from categorical raster - on polygons")
testthat::test_that("Test On points extraction", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  library(testthat)

  in_cat_rast <- in_rast[[1]] %>%
    raster::cut(breaks = c(1,10,20,30,40,50))
  out <- extract_rast(in_cat_rast, in_polys, id_field = "id",
                      verbose = FALSE, keep_null = TRUE,
                      rast_type = "categorical",
                      comp_freq = T)
  # Output is a list
  testthat::expect_is(out, "list")
  expect_equal(length(unique(out$stats$mode)), 4)
  expect_equal(length(unique(out$alldata$value)), 6)

})

context("Extract data from raster - on points")
testthat::test_that("Test On points extraction", {
  # skip_on_cran()
  skip_on_travis()
  library(sprawl.data)
  library(testthat)
  in_pts   <- read_vect(system.file("extdata/shapes","randpoints.shp",
                                    package = "sprawl.data"))
  in_file  <- system.file("extdata/MODIS_test", "EVIts_test.tif",
                          package = "sprawl.data")
  in_rast  <- read_rast(in_file)
  in_rast  <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8), year = 2013))
  out      <- extract_rast(in_rast[[1:2]], in_pts, id_field = "id",
                           verbose = FALSE, keep_null = T)  %>%
    tibble::as_tibble()

  # Output is a data frame
  testthat::expect_is(out, "data.frame")
  out_extract <- raster::extract(in_rast[[1:2]], as(in_pts, "Spatial")) %>%
    tibble::as_tibble()
  # Output is equal to raster::extract
  testthat::expect_equal(as.numeric(t(as.matrix(out[,3:103]))),
                         as.numeric(as.matrix(out_extract)))

  out <- extract_rast(in_rast[[1:2]], in_pts, id_field = "id",
                      verbose = F, keep_null = T, long_format = T)
  # On `long = TRUE` output is `sf`
  testthat::expect_is(out, "sf")

  # On `long = TRUE` but add_geom = FALSE output is not a `sf`
  out <- extract_rast(in_rast[[1:2]], in_pts, id_field = "id",
                      verbose = F, join_geom = F)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_error(st_geometry(out))
})


