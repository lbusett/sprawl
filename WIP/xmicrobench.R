

# outbig   <- extract_rast(in_rast, in_polys, verbose = FALSE,
#                          mode = "std",selbands = c(1,46),    maxchunk = 100E4, addgeom = F)
# in_rastfile = tempfile(fileext = ".tif")
# in_rast <- writeRaster(in_rast, in_rastfile)
#
# in_rast <- raster(in_rastfile)
# expect_warning(in_polys <- build_testshape(400) %>% sf::st_cast("MULTIPOLYGON"))
# in_rast   <- build_testraster(500,1000,100) %>%
#   raster::setZ(doytodate(seq(1,1000,10), 2013))
# in_rastfile = "inst/extdata/tif_world.tif"
# in_rast   <- sprawl::build_testraster(2000,2000,2)
# in_rast <- raster::writeRaster(in_rast, in_rastfile, overwrite = T, )
# in_rast <- raster::stack(in_rastfile)
# # in_polys <- create_fishnet(a, res(in_rast)[1])
# in_shape <- readshape("inst/extdata/grid_world.shp")
# npolys = 5
# in_polys <- in_shape[sample(1:5151, npolys),]

# in_rast <- writeRaster(in_rast, in_rastfile)
# in_rast <- stack(in_rastfile)
# in_polys <- build_testshape(100) %>% sf::st_cast("MULTIPOLYGON")


# npolys = 100
# in_polys <- in_shape[sample(1:5151, npolys),]
# sp_polys = as(in_polys, "Spatial")
#
# mb = microbenchmark::microbenchmark(
#   out_extract_rast  = extract_rast(in_rast, in_polys, verbose = F, long = F,
#                                 keep_null = T, addfeat = F, addgeom = F,
#                                 full_data = F, small = T,  comp_quant = FALSE, maxchunk = 1E7
#                                , ncores = 4),times = 1, unit = "s"),
#   out_extract = raster::extract(in_rast, sp_polys, na.rm = T, fun = mean),
#   times = 5, unit = "s")
# boxplot(mb)
# autoplot(mb) + theme_bw() + scale_y_continuous()
#
# mb = microbenchmark::microbenchmark(
#   # out_extract_rast  = extract_rast(in_rast, in_polys, verbose = T, long = F,
#   #                              keep_null = T, addfeat = F, addgeom = F,
#   #                              full_data = F, small = T,  comp_quant = FALSE
#   # ),
#   out_extract_rast2  = extract_rast(in_rast, in_polys, verbose = T, long = F,
#                                keep_null = T, addfeat = F, addgeom = F,
#                                full_data = F, small = T,  comp_quant = FALSE,
#                                maxchunk = 2E6
#   ),
#   out_extract_rast3  = extract_rast(in_rast, in_polys, verbose = T, long = F,
#                                 keep_null = T, addfeat = F, addgeom = F,
#                                 full_data = F, small = T,  comp_quant = FALSE,
#                                 maxchunk = 1E5),
#   times = 5, unit = "s")
# mb
# S
# profvis({out_extract_rast  = sprawl::extract_rast(in_rast, in_polys, verbose = T, long = F,
#                              keep_null = T, addfeat = F, addgeom = F,
#                              full_data = F, small = T,  comp_quant = FALSE, ncores = 4, maxchunk = 1E7)})
#
