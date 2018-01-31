#' @title extract raster data on polygons (helper for extract_rast)
#' @description FUNCTION_DESCRIPTION
#' @param in_vect PARAM_DESCRIPTION
#' @param in_vect_crop PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param na.value PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param er_opts PARAM_DESCRIPTION
#' @param parallel PARAM_DESCRIPTION
#' @param outside_feat PARAM_DESCRIPTION
#' @param verb_foreach `logical` if TRUE, verbose output is sent out from within the foreach cycle,
#' Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname er_polygons
#' @export
#' @importFrom data.table data.table rbindlist setkey as.data.table setcolorder
#' @importFrom dplyr case_when filter
#' @importFrom foreach foreach "%dopar%"
#' @importFrom gdalUtils gdal_translate
#' @importFrom raster res setZ raster writeRaster getValues extent yFromRow extract xyFromCell
#' @importFrom sf st_bbox st_as_sf st_geometry st_set_crs st_area
#' @importFrom sp proj4string
#' @importFrom tibble as_tibble
#' @importFrom velox velox
#' @importFrom parallel stopCluster
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr %>%

er_polygons <- function(in_vect_crop,
                        in_vect,
                        in_rast,
                        na.value,
                        seldates,
                        selbands,
                        n_selbands,
                        date_check,
                        er_opts,
                        outside_feat,
                        parallel = FALSE,
                        verb_foreach = FALSE) {

  geometry <- mdxtnq <- y_min <- .N <- .SD <- band <- area <- . <- NULL

  #   crop in_vect to in_rast extent if necessary and identify "removed"    ####
  #   features + Find names of the attribute fields of the original shapefile

  if (er_opts$verbose) message("extract_rast --> Cropping the zones object on ",
                               "extent of the raster")

  names_shp <- names(in_vect_crop)[!names(in_vect_crop) %in% c("mdxtnq", "geometry")] #nolint

  #   __________________________________________________________________________
  #   If `er_opts$rastres` is not null and is er_opts$smaller tha the       ####
  #   original resolution of in_rast, both `in_vect` and `in_rast` are
  #   "super-sampled to `er_opts$rastres` resolution (using nn resampling) prior
  #   to data extraction

  supersample <- 0
  if (is.null(er_opts$rastres)) {
    er_opts$rastres <- raster::res(in_rast)
  } else {
    if (length(er_opts$rastres) == 2 &
        length(is.finite(er_opts$rastres) == TRUE) &
        min(er_opts$rastres, na.rm = TRUE) > 0) {
      rastres <- er_opts$rastres
      supersample <- 1
      # ______________________________________________________________________
      # if "supersampling" requested, resample input raster to higher     ####
      # resolution (with nearest neaighbour).
      if (er_opts$verbose) {
        message("extract_rast --> Resmpling `in_rast` to ", rastres,
                "resolution")
      }
      rastinfo  <- get_rastinfo(in_rast)
      tempsuper <- tempfile(fileext = ".tif")
      in_rast   <- gdalUtils::gdal_translate(
        rastinfo$fnames[1], tempsuper, b = rastinfo$indbands, tr = er_opts$rastres,
        te = get_extent(in_rast)@extent, output_Raster = TRUE,
        multi = TRUE)
      names(in_rast) <- rastinfo$bnames
      if (date_check) in_rast <- raster::setZ(in_rast,rastinfo$Z)
    } else {
      warning("extract_rast--> Provided `er_opts$rastres = `", er_opts$rastres,
              " seems invalid. It will be reset to `in_rast` resolution")
      er_opts$rastres <- raster::res(in_rast)
    }
  }

  #   __________________________________________________________________________
  #   Rasterize the shape to a temporary file                               ####

  if (er_opts$verbose) {message("extract_rast --> Rasterizing shape")}
  if (er_opts$verbose) {message("extract_rast --> Writing temporary shapefile")}
  temp_shapefile <- tempfile(tmpdir = tempdir(), fileext = ".shp")
  write_shape(in_vect_crop, temp_shapefile, overwrite = TRUE)

  # then convert it to raster
  if (er_opts$verbose) {
    message("extract_rast --> Writing temporary rasterized shapefile")
  }
  temp_rasterfile <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  max_id <- max(in_vect_crop$mdxtnq)
  ot <- dplyr::case_when(
    (max_id <= 255) == 1 ~ "Byte",
    (max_id >= 255 & max_id < 65535) == 1 ~ "UInt16",
    (max_id >= 65536) == 1 ~ "UInt32"
  )

  te <- get_extent(in_rast)@extent
  rast_string <- paste("-a", "mdxtnq",
                       "-co" , "COMPRESS=DEFLATE",
                       "-co"  ,"NUM_THREADS=ALL_CPUS",
                       "-te", paste(te, collapse = " "),
                       "-tr", paste(er_opts$rastres, collapse = " "),
                       "-ot" , ot, sep = " ",
                       "-of GTiff",
                       temp_shapefile,
                       temp_rasterfile)

  system2(file.path(find_gdal(), "gdal_rasterize"), args = rast_string,
          stdout = NULL)

  rast_zoneobject <- raster::raster(temp_rasterfile)

  #  ___________________________________________________________________________
  #  setup the processing: initialize variables and foreach loop            ####

  # Setup the processing cluste using `sprawl_initcluster`
  if (parallel) {
    `%DO%` <- `%dopar%`
  } else {
    `%DO%` <- `%do%`
    er_opts$ncores <- 1
  }
  cl <- sprawl_initcluster(in_rast,
                           ncores = er_opts$ncores)
  cl_opts <- cl[[2]]

  if (er_opts$verbose) {
    message("extract_rast --> Extracting data from ", n_selbands,
            ifelse(date_check, " dates", " bands"),
            " - Please wait !")
    pbar <- utils::txtProgressBar(min = 0, max = n_selbands, initial = 0,
                                  style = 3)
  }
  #  ___________________________________________________________________________
  #  Extract data from in_rast - foreach cycle on selected bands of in_rast ####

  results <- foreach::foreach(band = seq_len(n_selbands),
                              .packages = c("gdalUtils", "raster", "dplyr",
                                            "tibble", "data.table", "sf",
                                            "velox", "sprawl"),
                              .verbose      = verb_foreach
  ) %DO%
  {
    # for (band in 1:1){

    all_data     <- list()
    coords       <- list()
    stat_data    <- list()
    temp_outdata <- list()
    selband      <- seldates[band]
    chunk_n_all  <- 1 # Counter for non-empty chunks for all_data
    chunk_n_summ <- 1 # Counter for non-empty chunks for all_data
    start_cell   <- 1
    in_band      <- in_rast[[band]]

    if (in_band@file@name == "") {
      temprastfile <- tempfile(fileext = ".tif")
      raster::writeRaster(in_band,
                          filename  = temprastfile,
                          options   = c("COMPRESS=DEFLATE"),
                          overwrite = TRUE)
      in_band <- raster::raster(temprastfile)
    }

    #TODO implement and test supersampling

    #   ____________________________________________________________________________
    #   if extraction needs to be done on chunks, identify the bboxes          ####
    #   bboxes of each polygon in the input (to be able to check if all
    #   pixels for that polygon has been already extracted)
    #
    if (cl_opts$n_chunks > 1) {

      if (er_opts$verbose) {
        message("extract_rast --> Computing bounding boxes of input polygons")
      }

      bboxes <- lapply(sf::st_geometry(in_vect_crop),
                       FUN = function(x) {
                         bb <- sf::st_bbox(x)
                         data.frame(y_min = bb[2], y_max = bb[4])
                       }) %>%
        data.table::rbindlist() %>%
        .[, "mdxtnq" := seq_len(dim(.)[1])] %>%
        data.table::setkey("mdxtnq")

      #
      #
      #
      # bboxes <- in_vect_crop[c("mdxtnq", "geometry")] %>%
      #   data.table::data.table() %>%
      #   sf::st_as_sf()
      # bboxes <- bboxes[, list(min_y = sf::st_bbox(geometry)[2],
      #                         max_y = sf::st_bbox(geometry)[4]),
      #                  by = "mdxtnq"]
    } else {
      bboxes <- in_vect_crop[c("mdxtnq", "geometry")]
    }

    # -_______________________________________________________________________
    # Perform data extraction (in chunks if number of cells greater then ####
    # max_chunk)

    for (chunk in seq_len(cl_opts$n_chunks)) {

      if (er_opts$verbose) message("Working on chunk: ", chunk,
                                   " of: ", cl_opts$n_chunks,
                                   " of band: ", selband)

      # Identify row numbers of the current "chunk" ----
      startrow   <- ifelse(chunk == 1,
                           1,
                           1 + (chunk - 1) * ceiling(cl_opts$nrows / cl_opts$n_chunks))
      chunkrows  <- ifelse(chunk != cl_opts$n_chunks,
                           ceiling(cl_opts$nrows / cl_opts$n_chunks),
                           (1 + cl_opts$nrows - startrow))
      endrow      <- startrow + chunkrows - 1
      ncells      <- cl_opts$ncols * chunkrows
      end_cell    <- start_cell + ncells - 1

      #   ____________________________________________________________________
      #   retrieve data of current "chunk" for the pixels included in the ####
      #   polygons and put it in all_data[[chunk_n]] (if not null)

      out_data  <- data.table::data.table(
        value  = as.numeric(raster::getValues(in_band, startrow, chunkrows)),
        cell   = seq(start_cell,end_cell),
        mdxtnq =  as.numeric(raster::getValues(rast_zoneobject, startrow,
                                               chunkrows)),
        key = "mdxtnq")
      out_data <- out_data[mdxtnq != 0]

      if (!is.null(na.value)) {
        which_na <- which(out_data$value == na.value)
        if (length(which_na != 0)) {
          out_data[which_na][["value"]] <- NA
        }
      }

      ext_chunk <- data.frame(x_min = raster::extent(in_band)[1],
                              x_max = raster::extent(in_band)[2],
                              y_min = raster::yFromRow(in_band, endrow),
                              y_max = raster::yFromRow(in_band, 1)
      )

      # If out_data not empty (i.e., at least one pixel of current chunk
      # belongs to a polygon), put out_data in all_data[[chubnk_n_all]],
      # then compute er_opts$summ_data
      if (dim(out_data)[1] > 0) {
        if (er_opts$full_data) {

          if (er_opts$join_geom) {
            # Here we create a temporary "velox" raster, allowing to quickly
            # compute coordinates and save coordinates for the chunk in the
            # "coords" list

            temp_velox <- velox::velox(matrix(nrow = chunkrows,
                                              ncol = cl_opts$ncols),
                                       extent = as.numeric(ext_chunk),
                                       res = er_opts$rastres,
                                       crs = sp::proj4string(in_band))

            # Note: here `out_data$cell - start_cell` makes so that the first
            # cell of the chunk ends up in position 1 in the temporary velox
            # (otherwise, out_data$cell is > than the number of cells in
            # tempvelox, after the first chunk)

            coords <- temp_velox$getCoordinates()[(out_data$cell - start_cell),]

            # add coordinates to the data table and remove the "cell" column
            out_data <- out_data[,c("cell", "x_coord", "y_coord") :=
                                   list(NULL, coords[,1], coords[,2])]

          } else {
            out_data <- out_data[, cell := NULL]
          }

          if (er_opts$full_data) {
            all_data[[chunk_n_all]] <- out_data
            chunk_n_all             <- chunk_n_all + 1
          }
        }
        if (er_opts$summ_data) {
          # __________________________________________________________________
          # verify if currently in out_data we have all the data for any  ####
          # of the polygons. In that case, compute the summary statistics
          # for those polygons, and remove their data from "out_data" if
          # full_data = FALSE (this to save memory on extraction on large
          # rasters)

          # get the extent of the area analysed so far on the basis of the
          # coordinates of the last row "loaded"

          tot_ext_y <- data.frame(
            y_min = (raster::yFromRow(in_band, endrow) - er_opts$rastres[1]/2), #nolint
            y_max = (raster::yFromRow(in_band, 1) + er_opts$rastres[1]/2))
          temp_outdata   <- data.table::rbindlist(list(temp_outdata,out_data))

          if (cl_opts$n_chunks > 1) {
            complete_polys <- bboxes[y_min >= tot_ext_y$y_min]
          } else {
            complete_polys <- bboxes
          }

          if (length(complete_polys$mdxtnq) != 0) {

            data_for_summary <- subset(temp_outdata, mdxtnq %in%
                                         unique(complete_polys$mdxtnq)) %>%
              data.table::setkey("mdxtnq")

            stat_data[[chunk_n_summ]] <- sprawl::summarize_data(
              data_for_summary,
              er_opts$rast_type,
              "mdxtnq",
              er_opts$comp_quant,
              er_opts$comp_freq,
              er_opts$FUN,
              er_opts$na.rm,
              selbands[band],
              selband)

            temp_outdata  <- temp_outdata[!(mdxtnq %in%
                                              unique(complete_polys$mdxtnq))]
            message(dim(temp_outdata))
            # If something was computed for stat_data, add 1 to the counter
            chunk_n_summ  <- chunk_n_summ + 1

          } # end IF on complete polygons
        } # end IF on compute SUMM
      } # end IF on at least one pixel extracted

      # Increment start_cell to get the start of the next chunk
      start_cell  <- end_cell + 1

    } # end cycle on chunks

    #   ______________________________________________________________________
    #   bind data from all chunks in `all_data`  and 'stat_data'          ####
    if (er_opts$full_data) {

      # if er_opts$na.rm == TRUE, remove nodata pixels from the out_data
      # table before storing it in all_data of the current chunk (useful
      # to reduce memory footprint on large areas with many NODATA)

      all_data <- data.table::rbindlist(all_data) %>%
        data.table::setkey("mdxtnq")
      if (er_opts$na.rm) {
        all_data <- na.omit(all_data, "value")
      }

    }

    if (er_opts$summ_data) {
      stat_data <- data.table::rbindlist(stat_data) %>%
        data.table::setkey("mdxtnq")
    }

    #   ______________________________________________________________________
    #   extract data for er_opts$small polygons if requested and          ####
    #   necessary using raster::extract

    if (er_opts$small &
        length(unique(stat_data$mdxtnq) != length(unique(in_vect_crop$mdxtnq)))) { #nolint

      miss_feat <- setdiff(unique(in_vect_crop$mdxtnq),
                           unique(stat_data$mdxtnq))
      for (mfeat in miss_feat) {

        poly_miss       <- in_vect_crop %>%
          dplyr::filter(mdxtnq == mfeat) %>%
          sf::st_as_sf() %>%
          as("Spatial")
        data_feat       <- raster::extract(in_rast[[band]], poly_miss,
                                           small = TRUE,
                                           method = "simple", df = TRUE,
                                           cellnumbers = TRUE)
        cell            <- data_feat[,2]
        miss_feat_data  <- data.table::data.table(value = data_feat[,3],
                                                  cell =  cell,
                                                  mdxtnq = mfeat)
        if (er_opts$join_geom) {
          coords <- raster::xyFromCell(in_band, cell)
          miss_feat_data <- miss_feat_data[, c("cell", "x_coord", "y_coord") := #nolint
                                             list(NULL, coords[,1], coords[,2])] #nolint
        } else {
          miss_feat_data <- miss_feat_data[,cell := NULL]
        }
        if (er_opts$full_data) {
          all_data        <- rbind(all_data, miss_feat_data)
        }
        # compute the summary statistics for the current small feature
        if (er_opts$summ_data) {

          miss_feat_stats <- summarize_data(miss_feat_data,
                                            er_opts$rast_type,
                                            "mdxtnq",
                                            er_opts$comp_quant,
                                            er_opts$comp_freq,
                                            er_opts$FUN,
                                            er_opts$na.rm,
                                            selbands[band],
                                            selband)

          stat_data       <- rbind(stat_data, miss_feat_stats)
        }
      }
    }

    # ________________________________________________________________________
    # if er_opts$full_data required, add some useful additional columns   ####
    # to all_data

    if (er_opts$full_data) {

      # this computes number of pixels per polygon and gives a sequential
      # number to each pixel in the polygon (https://goo.gl/c83Pfd)

      all_data <- all_data[, c("band_n", "date", "n_pix_val", "N") :=
                             list(band, seldates[band], .N, seq_len(.N)),
                           by = mdxtnq]
    }

    #  update progressbar
    if (er_opts$verbose) {
      Sys.sleep(0.001)
      utils::setTxtProgressBar(pbar, band)
    }

    #   ______________________________________________________________________
    #   return data processed by the "worker" (a.k.a. the "band" results) ####

    out <- list(alldata = all_data, stats = stat_data)
    # out
    return(out)

  } # End Foreach cycle on bands

  # if (parallel) {
    parallel::stopCluster(cl$clust)
  # }

  if (er_opts$verbose) message(
    "extract_rast --> Data extraction completed. Building outputs"
  )

  # ____________________________________________________________________________
  # End of data loading from in_rast. Now in all_data/stat_data we have all ####
  # values needed to build the output


  # ____________________________________________________________________________
  # if er_opts$keep_null selected and "outside features" found, replace   ####
  # in_vect_crop  with in_vect, so that later joins "include" the missing
  # features

  if (er_opts$keep_null & !is.null(outside_feat)) {
    in_vect_crop <- in_vect
  }

  #   __________________________________________________________________________
  #   Reshuffle output to build the stats output list                       ####

  if (er_opts$summ_data) {

    # "bind" the different bands
    stat_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 2))) %>%
      data.table::setkey("mdxtnq")

    # if er_opts$join_feat_tbl, merge the extracted data with the missing shapefile
    # features
    if (er_opts$join_feat_tbl) {

      stat_data <- stat_data[{data.table::as.data.table(in_vect_crop) %>%
          data.table::setkey("mdxtnq")}]
      # Add a column for area
      if (!er_opts$join_geom) {
        stat_data <- stat_data[, geometry := NULL]
      }

    } else {
      join_cols <- "mdxtnq"
      if (!is.null(er_opts$id_field)) {
        join_cols <- c(join_cols, eval(er_opts$id_field))
      }
      if (er_opts$join_geom) {
        join_cols <- c(join_cols,"geometry")
      }

      stat_data <- stat_data[{
        data.table::as.data.table(in_vect_crop)[, join_cols, with = FALSE] %>%
          data.table::setkey("mdxtnq")}]

    }
    # }
    if (!any(names_shp == "area")) {
      area_fld <- "area"
    } else {
      area_fld <- "area_sprawl"
    }

    if (er_opts$join_geom) {
      stat_data[[eval(area_fld)]] <- sf::st_area(stat_data$geometry)
    }

    # define the names and order of the output columns

    if (er_opts$rast_type == "continuous") {

      if (!is.null(er_opts$FUN)) {
        keep_cols <- c("mdxtnq", "band_n", "date", area_fld,
                       "n_pix", "n_pix_val", "myfun",
                       names_shp,
                       "geometry")
      } else {

        if (!er_opts$comp_quant) {
          keep_cols <- c("mdxtnq", "band_n", "date", area_fld,
                         "n_pix", "n_pix_val", "avg", "med", "sd", "min", "max",
                         names_shp,
                         "geometry")
        } else {
          keep_cols <- c("mdxtnq", "band_n", "date", area_fld,
                         "n_pix", "n_pix_val", "avg", "med", "sd", "min", "max",
                         "q01", "q05","q15", "q25", "q35", "q45", "q55", "q65",
                         "q75", "q85", "q95", "q99",
                         names_shp,
                         "geometry")
        }
      }
    } else {
      if (!er_opts$comp_freq) {
        keep_cols <- c("mdxtnq", "band_n", "date", area_fld,
                       "n_pix", "n_pix_val", "mode",
                       names_shp,
                       "geometry")
      } else {

        which_freqs <- grep("freq_", names(stat_data))
        keep_cols <- c(
          "mdxtnq", "band_n", "date", area_fld,
          "n_pix", "n_pix_val", "mode",
          gsub("mdtxtnQ_", "", names(stat_data)[which_freqs]), #nolint
          names_shp,
          "geometry")
        names(stat_data)[which_freqs] <-
          gsub("mdtxtnQ_", "", names(stat_data)[which_freqs])
      }
    }

    if (!er_opts$join_geom) {
      keep_cols <- keep_cols[which(!keep_cols %in% c(area_fld, "geometry"))]
      # if (!er_opts$join_feat_tbl) stat_data <- stat_data[, geometry := NULL]
    }

    if (!er_opts$join_feat_tbl) {
      keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    }

    if (!is.null(er_opts$id_field)) {
      stat_data <- stat_data[, mdxtnq := NULL]
      keep_cols <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }

    #   ________________________________________________________________________
    #   Build the final output and convert to tibble                        ####

    stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
      tibble::as_tibble()

    if (is.null(er_opts$id_field)) names(stat_data)[1] <- "id_feat"

    # If er_opts$join_geom, convert to a sf object
    if (er_opts$join_geom) {
      stat_data <- sf::st_as_sf(stat_data)
    }
  }

  #   __________________________________________________________________________
  #   Reshuffle output to build the alldata list (includes adding the       ####
  #   "point" coordinates and transforming to a `sf` object

  if (er_opts$full_data) {

    # "bind" the different bands
    all_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 1))) %>%
      data.table::setkey("mdxtnq")
    sf::st_geometry(in_vect_crop) <- NULL

    # if er_opts$join_feat_tbl, merge the extracted data with the missing shapefile
    # features

    if (er_opts$join_feat_tbl) {
      all_data <- merge(all_data, in_vect_crop, by = "mdxtnq", all.y = TRUE)
    } else {
      if (!is.null(er_opts$id_field)) {

        all_data <- all_data[{
          data.table::as.data.table(
            in_vect_crop[,c(eval(er_opts$id_field), "mdxtnq")]) %>%
            data.table::setkey("mdxtnq")}]}
    }

    # define the order of the output columns
    keep_cols <- c("mdxtnq", "band_n", "date", "n_pix_val", "N",
                   "value",
                   names_shp,
                   "x_coord", "y_coord")

    if (!er_opts$join_feat_tbl) {
      if (is.null(er_opts$id_field)) {
        keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      } else {
        keep_cols <- keep_cols[which((!keep_cols %in% names_shp) &
                                       (keep_cols != er_opts$id_field))]
      }
    }
    # if join_geom is FALSE, remove coordinates
    if (!er_opts$join_geom) {
      keep_cols <- keep_cols[which(!keep_cols %in% c("x_coord", "y_coord"))]
    }
    if (!is.null(er_opts$id_field)) {
      keep_cols    <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }

    # build the final output and convert to tibble
    all_data  <- all_data[ , .SD, .SDcols = keep_cols] %>%
      tibble::as_tibble()

    # If er_opts$join_geom, convert to a sf object
    if (er_opts$join_geom) {
      all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"),
                                na.fail = FALSE) %>%
        sf::st_set_crs(get_proj4string(in_rast))
    }
    if (is.null(er_opts$id_field)) names(all_data)[1] <- "id_feat"
  }

  #   __________________________________________________________________________
  #   Final cleanup                                                         ####

  # if dates were not passed, then change the name of column 3 to "band_name"
  if (!date_check) {
    if (er_opts$summ_data) names(stat_data)[3] <- "band_name"
    if (er_opts$full_data) names(all_data)[3]  <- "band_name"
  }

  if (!er_opts$full_data) all_data  <- NULL
  if (!er_opts$summ_data) stat_data <- NULL

  # create the final output list
  ts_out <- list(stats = stat_data, alldata = all_data)

  file.remove(temp_rasterfile)
  file.remove(temp_shapefile)

  return(ts_out)
}
