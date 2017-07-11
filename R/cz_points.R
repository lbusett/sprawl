#' @title cz_points
#' @description FUNCTION_DESCRIPTION
#' @param in_vect_zones `sf` object on which features the zonal statistics has to be computed
#' @param in_rast single- or multi-band `rasterStack` object containing values to be extracted and
#'  summarized
#' @param n_selbands aa
#' @param seldates aaa
#' @param date_check zzz
#' @inheritParams comp_zonal
#' @return OUTPUT_DESCRIPTION
#'
#' @importFrom dplyr mutate right_join select
#' @importFrom raster extract
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble add_column as_tibble as_data_frame
#' @importFrom tidyr gather
#' @importFrom magrittr %>%

cz_points <- function(in_vect_zones,
                      in_rast,
                      n_selbands,
                      selbands,
                      seldates,
                      id_field,
                      date_check,
                      long,
                      verbose,
                      addfeat,
                      addgeom,
                      keep_null) {

  if (verbose) message("comp_zonal --> Cropping the zones object on extent of the raster")
  crop               <- cz_crop_object(in_vect_zones, in_rast, id_field, verbose)
  in_vect_zones_crop <- crop$in_vect_zones_crop
  outside_feat     <- crop$outside_feat

  if (verbose) { message("comp_zonal --> On point and lines shapefiles, the standard `extract` function is used. This could be slow !")}

  tserie <- matrix(nrow = n_selbands, ncol = dim(in_vect_zones_crop)[1])

  for (band in seq_len(n_selbands)) {
    selband <- selbands[band]
    if (verbose) {
      message(paste0("comp_zonal --> Extracting data from ", ifelse(date_check, "date: ", "band: "),
                     seldates[band]))
    }
    tserie[band,] <- in_rast[[selband]] %>%
      raster::extract(as(in_vect_zones_crop, "Spatial"))
  }

  # add the date/band name column
  tserie <- tserie %>%
    as.data.frame() %>%
    cbind(seldates,.) %>%
    tibble::add_column(band_n = seq(dim(tserie)[1]), .after = 1)

  all_feats <- as.character(in_vect_zones_crop$mdxtnq)

  if (date_check) {
    names(tserie) <- c("date", "band_n", all_feats)
  } else {
    names(tserie) <- c("band_name", "band_n", all_feats)
  }


  #   ____________________________________________________________________________
  #   If some features are outside the raster and add_null = TRUE, add empty  ####
  #   columns for the missing features at the end of the table

  if (keep_null & !is.null(outside_feat)) {

    for (feat in 1:length(outside_feat$outside_ids)) {

      addcol <- data.frame(data = rep(NA, length(tserie[,1])))
      names(addcol)[1] <- outside_feat$outside_ids[feat]
      tserie = cbind(tserie, addcol)
    }

  } else {
    in_vect_zones = in_vect_zones_crop
  }

  # if long format is selected, reshape
  if (long) {

    tserie <- tserie %>%
      tidyr::gather(mdxtnq, value, -1, -2) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(mdxtnq = as.numeric(as.character(mdxtnq))) %>%
      dplyr::right_join(tibble::as_data_frame(in_vect_zones), by = "mdxtnq") %>%
      sf::st_as_sf()

    if (!is.null(id_field)) {
      tserie <- tserie %>%
        dplyr::select(-mdxtnq) %>%
        select(c(1,2, which(names(.) == id_field),
                 which(!(names(.) %in% c("date", "band_n", eval(id_field))))))
    } else {
      names(tserie)[3] <- "id_feat"
    }
    if (!addgeom) {
      sf::st_geometry(tserie) = NULL
    }

    if (!addfeat) {
      sf::st_geometry(tserie) = NULL
      tserie <- tserie[c(1:4),]
    }

  } else {
    if (!is.null(id_field) ){
      if (keep_null) {
        names(tserie)[3:(dim(tserie)[2])] <- t(rbind(tibble::as_data_frame(in_vect_zones[, eval(id_field)])[,1]))
      } else {
        names(tserie)[3:dim(tserie)[2]] <- t(rbind(tibble::as_data_frame(in_vect_zones_crop[, eval(id_field)])[,1]))
      }
    }

  }
  return(tibble::as_tibble(tserie))
}
