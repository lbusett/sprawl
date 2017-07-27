  #   ____________________________________________________________________________
  #   correct the extent for gdal_rasterize to get                            ####
  #   an identically sized output
  # te <- te - c(-raster::res(in_rast)[1], -raster::res(in_rast)[1], 0,  0)
  # te <- raster::extent(in_rast)[c(1, 3, 2, 4)][] - c(0, -res(in_rast)[1], res(in_rast)[1], 0)

  #   ____________________________________________________________________________
  #   Rasterize the mask shapefile                                            ####

  # temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  # rasterize_string <- paste("-at",
  #                           "-burn 1",
  #                           "-a_nodata", out_nodata,
  #                           "-te", paste(te, collapse = " "),
  #                           "-tr", paste(raster::res(in_rast), collapse = " "),
  #                           "-ot Byte",
  #                           temp_shapefile,
  #                           temp_rastermask)

  # system2(file.path(find_gdal(), "gdal_rasterize"),
  #         args = rasterize_string, stdout = NULL)
