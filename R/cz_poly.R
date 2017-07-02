#' @title cz_poly
#' @description FUNCTION_DESCRIPTION
#' @param zone_object PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param n_selbands aa
#' @param seldates aaa
#' @param zone_type zone_type
#' @param date_check zone_type
#' @inheritParams comp_zonal
#' @return OUTPUT_DESCRIPTION

#' @importFrom data.table data.table rbindlist setkey
#' @importFrom dplyr case_when group_by mutate select right_join
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom raster res extent crop getValues
#' @importFrom sf st_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom stats median sd quantile
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr %>%
cz_poly <- function(zone_object,
                    in_rast,
                    rastres,
                    maxchunk,
                    n_selbands,
                    selbands,
                    seldates,
                    id_field,
                    date_check,
                    long,
                    verbose,
                    comp_quant,
                    small,
                    addfeat,
                    addgeom,
                    keep_null,
                    zone_type,
                    summ_data,
                    full_data) {


  #   ____________________________________________________________________________
  #   crop zone_object to in_rast extent if necessary and identify "removed"   ####
  #   features
  #
  crop             <- cz_crop_object(zone_object, in_rast, id_field)
  zone_object_crop <- crop$zone_object_crop
  outside_feat     <- crop$outside_feat

  #   ____________________________________________________________________________
  #   If zone_object is not already a raster file, then rasterize the polygon  ####
  #   shapefile. (If `rastres` is not null and is smaller tha the original
  #   resolution of in_rast, both `zone_object` and `in_rast` are "super-sampled
  #   to `rastres` resolution (using nn resampling)
  #

  if (zone_type != "rastobject") {
    supersample <- 0
    if (is.null(rastres)) {
      rastres = raster::res(in_rast)
    } else {
      if (length(rastres) == 2 & length(is.finite(rastres) == TRUE) & min(rastres, na.rm = T) > 0) {
        rastres     <- rastres
        supersample <- 1
      } else {
        warning("comp_zonal --> Provided `rastres = `", rastres, " seems invalid. It will be reset to `in_rast` resolution")
        rastres = raster::res(in_rast)
      }
    }

    # First of all, rasterize the shape to a temporary file

    if (verbose) {message("comp_zonal --> Rasterizing shape")}
    if (verbose) {message("comp_zonal --> Writing temporary shapefile")}
    temp_shapefile = tempfile(tmpdir = tempdir(), fileext = ".shp")
    writeshape(zone_object_crop, temp_shapefile, overwrite = TRUE)

    # then convert it to raster
    if (verbose) {(message("comp_zonal --> Writing temporary rasterized shapefile"))}
    temp_rasterfile = tempfile(tmpdir = tempdir(), fileext = ".tiff")
    max_id <- max(zone_object_crop$mdxtnq)
    ot <- dplyr::case_when(
      (max_id <= 255) == 1 ~ "Byte",
      (max_id >= 255 & max_id < 65535) == 1 ~ "Int16",
      (max_id >= 65536) == 1 ~ "Int32"
    )
    rastzone_object <- gdalUtils::gdal_rasterize(temp_shapefile,
                                                 temp_rasterfile,
                                                 tr = rastres,
                                                 te = raster::extent(in_rast)[c(1, 3, 2, 4)],
                                                 a  = "mdxtnq",
                                                 ot = ot,
                                                 output_Raster = TRUE)
  } else {
    # if input zone_object already a raster (e.g., a fishnet) just crop it on raster extent
    rastzone_object = raster::crop(zone_object, raster::extent(in_rast[[1]]))
    if (!is.null(rastres)) {"comp_zonal --> `rastres` is ignored since `zone_object` is already a
      raster file. `in_rast` will be automatically supersampled to `zone_object` resolution prior
      to data extraction ! "
    }
    supersample <- 1
    rastres <- res(rastzone_object)
    warning("")
  }

  #   ____________________________________________________________________________
  #  setup variables to store outputs                                         ####

  # sf::st_geometry(zone_object_crop) <- NULL
  # Setup chunks and objects to store results ----
  n_cells   <- nrow(rastzone_object) * ncol(rastzone_object)
  # ncols     <- ncol(rastzone_object)
  n_chunks  <- floor(n_cells / maxchunk)


  tserie        <- list()
  tserie_full   <- list()
  # zone_ids  <- unique(all_data$zones)
  # if (length(id_field) == 1) {
  #   feat_names <- as.character(zone_object_crop[, eval(id_field)])
  # } else {
  #   feat_names <- as.character(zone_object_crop$mdxtnq)
  # }



  if (verbose) {
    message("comp_zonal --> Extracting data from ", n_selbands, ifelse(date_check, " dates", "bands"), " - Please wait !")
    pb <- utils::txtProgressBar(min = 0, max = n_selbands,
                                char = "=",
                                style = 3)
  }

  #   ____________________________________________________________________________
  #   Extract data from in_rast - cycle on in_rast                              ####

  for (band in seq_len(n_selbands)) {

    all_data  <- list()
    part_data <- list()
    selband = seldates[band]
    in_band <- in_rast[[selbands[band]]]

    if (supersample) {

      temprast <- tempfile(fileext = ".tif")
      writeRaster(in_band, temprast)
      tempsuper <- tempfile(fileext = ".tif")
      in_band <- gdalUtils::gdalwarp(temprast, tempsuper, tr = rastres,
                                     te = raster::extent(in_rast)[c(1, 3, 2, 4)], output_Raster = TRUE ,
                                     multi = TRUE)

    }

    if (n_chunks > 1) {
      chunk_n <- 1 # Counter for non-empty chunks
      # Perform data extraction in chunks if number of cells greater then max_chunk ----
      for (chunk in seq_len(n_chunks)) {
        if (verbose) message("Working on chunk: ", chunk, " of: ", n_chunks)

        # Import data chunk ----
        # browser()
        startrow <- ifelse(chunk == 1, 1, 1 + (chunk - 1) * ceiling(nrow(rastzone_object) / n_chunks))

        nrows    <- ifelse(chunk != n_chunks,
                           ceiling(nrow(rastzone_object) / n_chunks),
                           (1 + nrow(rastzone_object) - startrow)
        )

        #   ____________________________________________________________________________
        #   put current chunk in "all_data"                                          ####
        values <- raster::getValues(in_band, startrow,
                                    ifelse(chunk == 1, (nrows - 1), nrows))
        out_data <- data.table::data.table(
          value  =  as.numeric(values),
          mdxtnq =  as.numeric(raster::getValues(rastzone_object, startrow,
                                                 ifelse(chunk == 1, (nrows - 1), nrows))),
          key = "mdxtnq") %>%
          subset(mdxtnq != 0)  %>%   # remove data outside polygons (== zone_id = 0 OR NA)
          subset(!is.na(mdxtnq))

        if (dim(out_data)[1] > 0) {
          if (full_data) all_data[[chunk_n]]  <- out_data
          if (summ_data) part_data[[chunk_n]] <- fast_summ(out_data, "mdxtnq", comp_quant, selbands[band], selband)
          chunk_n <- chunk_n + 1
        }
        gc()
      }

      #   ____________________________________________________________________________
      #   Join all chunks in `all_data`                                           ####

      if (full_data){
        all_data <- data.table::rbindlist(all_data) %>%
          data.table::setkey("mdxtnq")
      }

      if (summ_data) {
        tserie[[band]] <- data.table::rbindlist(part_data) %>%
          data.table::setkey("mdxtnq")
      }

    } else {
      #   ____________________________________________________________________________
      #   # if chunking not needed, load the full table of data in `all_data`     ####

      values   = raster::getValues(in_band)
      all_data <- data.table::data.table(
        value = as.numeric(values),
        mdxtnq = as.numeric(raster::getValues(rastzone_object)),
        key = "mdxtnq") %>%
        subset(mdxtnq != 0) %>%
        subset(!is.na(mdxtnq)) # remove data outside polygons (== zone_id = 0 OR NA)

      # Apply the aggregation function if needed, otherwise just extract all pixrels
      if (summ_data) {
        tserie[[band]] <- fast_summ(all_data, "mdxtnq", comp_quant, selbands[band], selband)
      }

    }

    # End of data loading from in_rast. Now in all_data we have values for the
    # zone_id to extract



    #   ____________________________________________________________________________
    #   if full_data, extract all pixels values and convert to tibble           ####

    if (full_data) {

      tserie_full[[band]] <- all_data %>%
        dplyr::group_by(mdxtnq) %>%
        dplyr::mutate(N_PIX   = seq(along = value),
                      date    = selband,
                      band_n  = selbands[band]) %>%
        dplyr::select(mdxtnq, band_n, date, N_PIX, value)
    }

    ##  ............................................................................
    ##  update progressbar                                                      ####
    if (verbose) {
      Sys.sleep(0.001)
      utils::setTxtProgressBar(pb, band)
    }

  }
  browser()
  #   ____________________________________________________________________________
  #   Reshuffle output to build the ts_summ list                              ####

  if (keep_null & !is.null(outside_feat)) {
    zone_object_crop <- zone_object
  }

  if (summ_data) {
    tserie <- data.table::rbindlist(tserie)

    # if `id_field` is missing or not valid, create a new column named "id_feat" and place it in
    # first place



    if (is.null(id_field)) {
      names_tmp <- names(tserie)
      tserie <- dplyr::right_join(tserie, zone_object_crop, by = "mdxtnq") %>%
        tibble::as_tibble() %>%
        select(c(1,2,3, (10:(length(.)-1)), 4:9, length(.)))

      # If addfeat == FALSE, remove the columns present only in the shapefile
      if (!addfeat) {
        tserie <- dplyr::select(tserie, which(names(tserie) %in% names_tmp)) %>%
          tibble::as_tibble()
      }
      names(tserie)[1] = "id_feat"
    } else {

      # if `id_field` is valid, find it and put it in first place in the output
      # first place
      #

      tserie <- tserie %>%
        dplyr::right_join(zone_object_crop, by = "mdxtnq") %>%
        tibble::as_tibble() %>%
        dplyr::select(-mdxtnq) %>%
        dplyr::select(which(names(.) == id_field),
                      1:2,
                      (9:(length(.) - 1))
                      , 3:8,
                      length(.))

      # If addfeat == FALSE, remove the columns present only in the shapefile
      if (!addfeat) {
        names_shp     <- names(zone_object_crop)[names(zone_object_crop) != "mdxtnq" &
                                                   names(zone_object_crop) != id_field ]
        keep_names    <- which(names(tserie) %in% setdiff(names(tserie), names_shp))
        tserie        <- dplyr::select(tserie, keep_names)
      }
    }

    # If `long`, reshape the output to a long table
    if (long) {

      skip_cols <- ifelse(comp_quant,17, 7)
      if (!is.null(id_field)){
        idcols    <- c(seq(1,length(tserie) - skip_cols), length(tserie))
        n_adds <- which(names(tserie) == "N") - 4
      } else {
        idcols    <- c(seq(1,length(tserie) - skip_cols), length(tserie))
        n_adds <- which(names(tserie) == "N") - 4
      }
      # browser()
      tserie <- tserie %>%
        tidyr::gather(variable, value, -idcols) %>%
        select(1:(3 + n_adds),
               ((length(.) - 1):length(.)),
               (length(.) - 2)) %>%
        dplyr::mutate(variable = as.factor(variable))
    }
  }

  #   ____________________________________________________________________________
  #   Reshuffle the output to build the tseries_full output list                ####

  if (full_data) {
    tserie_full <- data.table::rbindlist(tserie_full)

    if (is.null(id_field)) {
      names_tmp <- names(tserie_full)
      tserie_full <- dplyr::right_join(tserie_full, zone_object_crop, by = "mdxtnq") %>%
        tibble::as_tibble()

      # If addfeat == FALSE, remove the columns present only in the shapefile
      if (!addfeat) {
        tserie_full <- dplyr::select(tserie_full, which(names(tserie_full) %in% names_tmp)) %>%
          tibble::as_tibble()
      }
      names(tserie_full)[1] = "id_feat"
    } else {

      # if `id_field` is valid, find it and put it in first place in the output
      # first place
      tserie_full <- dplyr::right_join(tserie_full, zone_object_crop, by = "mdxtnq")

      tserie_full <- tserie_full %>%
        dplyr::select(which(names(tserie_full) == id_field),
                      which(names(tserie_full) != "mdxtnq")) %>%
        tibble::as_tibble()


      # If addfeat == FALSE, remove the columns present only in the shapefile
      if (!addfeat) {
        names_shp     <- names(zone_object_crop)[names(zone_object_crop) != "mdxtnq" &
                                                   names(zone_object_crop) != id_field ]
        keep_names    <- which(names(tserie_full) %in% setdiff(names(tserie_full), names_shp))
        tserie_full        <- dplyr::select(tserie_full, keep_names)
      }
    }



  }

  # if dates were not passed, then change the name of column 3 to "band_name"
  if (!date_check) {
    if (summ_data) names(tserie)[3]      <- "band_name"
    if (full_data) names(tserie_full)[3] <- "band_name"
  }

  if (!full_data) {
    tserie_full <- NULL
  }

  if (!summ_data) {
    tserie <- NULL
  }

  ts_out <- list(ts_summ = tserie, ts_full = tserie_full)

  # TODO: Cleanup of temporary files !!!!
  # file.remove(temp_rasterfile)
  # file.remove(temp_shapefile)
  #

  return(ts_out)

}
