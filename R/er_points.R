#' @title helper for `extract_rast`` for points extraction
#' @description FUNCTION_DESCRIPTION
#' @param in_vect_crop PARAM_DESCRIPTION
#' @param in_vect PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param er_opts PARAM_DESCRIPTION
#' @param outside_feat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname er_points
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom dplyr mutate right_join select
#' @importFrom raster extract
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble add_column as_tibble as_data_frame
#' @importFrom tidyr gather
#' @importFrom magrittr %>%

er_points <- function(in_vect_crop,
                        in_vect,
                        in_rast,
                        seldates,
                        selbands,
                        n_selbands,
                        date_check,
                        er_opts,
                        outside_feat) {

  # To avoid NOTES on check
  mdxtnq <- value  <- . <- NULL

  if (er_opts$verbose) {
    message("extract_rast --> Cropping the zones object on extent ",
            "of the raster")
  }

  if (er_opts$verbose) {
    message("extract_rast --> On point and lines vector objects, the standard",
            "extract function is used. This could be slow!")
  }
  tserie <- matrix(nrow = n_selbands, ncol = dim(in_vect_crop)[1])

  for (band in seq_len(n_selbands)) {
    selband <- selbands[band]
    if (er_opts$verbose) {
      message("extract_rast --> Extracting data from ",
              ifelse(date_check, "date: ", "band: "),
              seldates[band])
    }

    tserie[band,] <- in_rast[[selband]] %>%
      raster::extract(as(in_vect_crop, "Spatial"))
  }

  # add the date/band name column
  tserie <- tserie %>%
    as.data.frame() %>%
    cbind(seldates,.) %>%
    tibble::add_column(band_n = seq(dim(tserie)[1]), .after = 1)

  all_feats <- as.character(in_vect_crop$mdxtnq)

  if (date_check) {
    names(tserie) <- c("date", "band_n", all_feats)
  } else {
    names(tserie) <- c("band_name", "band_n", all_feats)
  }

  # ___________________________________________________________________________
  # If some features are outside the raster and add_null = TRUE, add empty ####
  # columns for the missing features at the end of the table

  if (er_opts$keep_null & !is.null(outside_feat)) {

    for (feat in seq_along(outside_feat$outside_ids)) {

      addcol <- data.frame(data = rep(NA, length(tserie[,1])))
      names(addcol)[1] <- outside_feat$outside_ids[feat]
      tserie <- cbind(tserie, addcol)
    }

  } else {
    in_vect <- in_vect_crop
  }

  # if long format is selected, reshape
  if (er_opts$long_format) {

    tserie <- tserie %>%
      tidyr::gather(mdxtnq, value, -1, -2) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(mdxtnq = as.numeric(as.character(mdxtnq))) %>%
      dplyr::right_join(tibble::as_data_frame(in_vect), by = "mdxtnq") %>%
      sf::st_as_sf()

    if (!is.null(er_opts$id_field)) {
      tserie <- tserie %>%
        dplyr::select(-mdxtnq) %>%
        dplyr::select(
          c(1,2, which(names(.) == er_opts$id_field),
            which(!(names(.) %in% c("date", "band_n", eval(er_opts$id_field)))))
          )
    } else {
      names(tserie)[3] <- "id_feat"
    }
    if (!er_opts$join_geom) {
      sf::st_geometry(tserie) <- NULL
    }

    if (!er_opts$join_feat_tbl) {
      sf::st_geometry(tserie) <- NULL
      tserie <- tserie[c(1:4),]
    }

  } else {
    if (!is.null(er_opts$id_field) ) {
      if (er_opts$keep_null) {
        names(tserie)[3:(dim(tserie)[2])] <- in_vect[[eval(er_opts$id_field)]]
      } else {
        names(tserie)[3:(dim(tserie)[2])] <- in_vect_crop[[eval(er_opts$id_field)]]
      }
    }

  }
  return(tibble::as_tibble(tserie))
}
