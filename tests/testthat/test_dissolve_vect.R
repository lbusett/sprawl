context("dissolve_vect")


test_that("dissolve_vect works as expected", {
  library(sprawl.data)
    in_vect <- get(load(system.file("extdata/shapes", "poly_lomb.RData",
                                    package = "sprawl.data")))
    dissolve_var     <- "NAME_2"
   expect_warning(out_shape <- dissolve_vect(in_vect, dissolve_var))
   expect_is(out_shape, "sf")
   expect_equal(sum(in_vect$population), sum(out_shape$population))
   expect_warning(out_shape <- dissolve_vect(in_vect,
                                             dissolve_var,
                                             out_file = tempfile(fileext = ".shp")))
})
