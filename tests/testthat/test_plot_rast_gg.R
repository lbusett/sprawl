context("plot_rast_gg")


test_that("plot_rast_gg works as expected", {

    in_rast <- raster::stack(system.file("extdata/OLI_test", "oli_multi_1000.tif",
                                       package = "sprawl.data"))[[1]]

    plot_rast_gg(in_rast, scalebar_dist = 20)

})
