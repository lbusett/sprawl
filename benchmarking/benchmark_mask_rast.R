oli_rast     <- raster::stack(system.file("extdata/OLI_test", "oli_multi.tif", package = "sprawl.data"))

benches <- list()
rast_dims <- c(1000, 2000, 4000, 8000)
# rast_dims <- c(2000)
n_polys   <- c(1,10, 20)
n_bands   <- c(6)
index <- 0
for (dims in rast_dims) {

  temp_poly   <- create_fishnet(oli_rast, pix_for_cell = c(dims, dims))[1,]
  in_rast_dim <- crop_rast(oli_rast, temp_poly, out_type = "rastobject")

  for (polys in n_polys) {

    for (bands in n_bands) {

      in_rast <- raster::stack(in_rast_dim[[1:bands]])
      message(polys, " ", bands )
      index <- index + 1

      in_vect  <- create_fishnet(in_rast, pix_for_cell = dims[1]/10) %>%
        dplyr::sample_n(polys)
      in_vect  <- sf::st_set_crs(in_vect, get_projstring(in_rast_dim))
      sp_polys <- as(in_vect, "Spatial")

      bench    <- microbenchmark::microbenchmark(
        "raster::mask" = {out_mask_raster  <- raster::mask(
          in_rast_dim, sp_polys, filename = tempfile(fileext = ".tif"), co = "COMPRESS=LZW")
        # out_mask_raster = NULL
        # gc()},
        # "sprawl::mask_rast" = {out_mask_raster  <- mask_rast(in_rast_dim, in_vect, crop = FALSE)
        # out_mask_raster = NULL
        },
        "sprawl::mask_rast_new" = {out_mask_raster  <- mask_rast_new(in_rast, in_vect, crop = FALSE, compress = "LZW",
                                                                     out_nodata = 0, parallel = F)},
        "sprawl::mask_rast_new_par" = {out_mask_raster  <- mask_rast_new(in_rast, in_vect, crop = FALSE, compress = "LZW",
                                                                     out_nodata = 0, parallel = T)},
        # # out_mask_raster = NULL
        # gc()},
        times = 1)
      bench <- summary(bench) %>%
        dplyr::mutate(n_bands = bands, n_polys = polys, dims = dims)
      benches[[index]] <- bench

    }

  }
}

gdal_setInstallation()
gdalUtils::gdalbuildvrt(tmprast, "D:/Temp/buttami/temp.vrt")
