#' @title extract raster data on polygons (helper for extract_rast)
#' @description FUNCTION_DESCRIPTION
#' @param in_vect PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param er_opts PARAM_DESCRIPTION
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
#'  melt
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr case_when filter
#' @importFrom foreach foreach %dopar%
#' @importFrom gdalUtils gdalwarp
#' @importFrom raster res extent raster nrow ncol writeRaster getValues yFromRow
#'  extract xyFromCell
#' @importFrom sf st_bbox st_as_sf st_geometry st_set_crs
#' @importFrom sp proj4string
#' @importFrom tibble as_tibble
#' @importFrom velox velox
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr %>%

er_polygons <- function(in_vect,
                        in_rast,
                        seldates,
                        selbands,
                        n_selbands,
                        date_check,
                        er_opts,
                        verb_foreach = FALSE) {

  geometry <- mdxtnq <- min_y <- .N <- .SD <- band <- NULL

  #   __________________________________________________________________________
  #   crop in_vect to in_rast extent if necessary and identify "removed"    ####
  #   features + Find names of the attribute fields of the original shapefile

  if (er_opts$verbose) message("extract_rast --> Cropping the zones object on ",
                               "extent of the raster")

  #TODO  Do this only if extent of zone object is larger than that of the raster
  # (in any direction)

  diff_ext <- as.numeric((extent(in_rast) + 461)[] ) -
    as.numeric(sf::st_bbox(in_vect)[c(1,3,2,4)])

  if (max(abs(diff_ext) >= 1000)) {
    crop <- er_crop_object(in_vect, in_rast, er_opts$id_field, er_opts$verbose)
    in_vect_crop <- crop$in_vect_crop
    outside_feat <- crop$outside_feat
  } else {
    in_vect_crop <- in_vect
    outside_feat       <- NULL
  }

  names_shp <- names(in_vect_crop)[!names(in_vect_crop) %in% c("mdxtnq", "geometry")] #nolint

  #TODO substitute with call to `crop_rast`
  # ____________________________________________________________________________
  # find a correct cropping bounding box which allows to not "move"         ####
  # the corners while creating a vrt file

  vect_bbox <- sf::st_bbox(in_vect_crop)
  rast_bbox <- raster::extent(in_rast)[c(1,3,2,4)]

  col_coords <- rast_bbox[1] + raster::res(in_rast)[1] * seq_len(dim(in_rast)[2]) #nolint
  row_coords <- rast_bbox[2] + raster::res(in_rast)[2] * seq_len(dim(in_rast)[1]) #nolint
  start_x <- ifelse((rast_bbox[1] < vect_bbox[1]),
                    col_coords[data.table::last(which(col_coords <= vect_bbox[1])) - 1], #nolint
                    rast_bbox[1])
  end_x   <- ifelse((rast_bbox[3] > vect_bbox[3]),
                    col_coords[data.table::last(which(col_coords <= vect_bbox[3])) + 1], #nolint
                    rast_bbox[3])
  start_y <- ifelse((rast_bbox[2] < vect_bbox[2]),
                    row_coords[data.table::last(which(row_coords <= vect_bbox[2])) - 1], #nolint
                    rast_bbox[2])
  end_y  <- ifelse((rast_bbox[4] > vect_bbox[4]),
                   row_coords[data.table::last(which(row_coords <= vect_bbox[4])) + 1], #nolint
                   rast_bbox[4])

  te         <- c(start_x, start_y, end_x, end_y)

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
      er_opts$rastres <- er_opts$rastres
      supersample <- 1
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

  rast_string <- paste("-a", "mdxtnq",
                       "-co" , "COMPRESS=DEFLATE",
                       "-co"  ,"NUM_THREADS=ALL_CPUS",
                       "-te", paste(te, collapse = " "),
                       "-tr", paste(er_opts$rastres, collapse = " "),
                       "-ot" , ot, sep = " ",
                       "-of GTiff",
                       temp_shapefile,
                       temp_rasterfile)

  system2(file.path(find_gdal(), "gdal_rasterize"),
          args = rast_string, stdout = NULL)

  rast_zoneobject <- raster::raster(temp_rasterfile)

  #  ___________________________________________________________________________
  #  setup the processing: initialize variables and foreach loop            ####

  # Setup the number of cores: defaults to available cores - 2, but up to a
  # maximum of 8. If user-provided er_opts$ncores is greater than available
  # cores - 2 or greater than 8 er_opts$ncores is re-set to the minimum between
  # those two. If selbands < er_opts$ncores, use only selbands cores

  #TODO: substitute with call to `sprawl_initcluster`
  if (is.null(er_opts$ncores)) {
    er_opts$ncores <- parallel::detectCores() - 2
  }
  er_opts$ncores <- min(c(er_opts$ncores, (parallel::detectCores() - 2)), 8)
  if (n_selbands < er_opts$ncores) (er_opts$ncores <- n_selbands)

  # cl      <- parallel::makeCluster(er_opts$ncores, outfile = "")
  cl <- parallel::makeCluster(er_opts$ncores)
  doSNOW::registerDoSNOW(cl)

  # Initialize other variables and progress bar
  er_opts$maxchunk  <- er_opts$maxchunk/er_opts$ncores
  nrows             <- raster::nrow(rast_zoneobject)
  ncols             <- raster::ncol(rast_zoneobject)
  n_cells           <- nrows * ncols
  n_chunks          <- floor(n_cells / er_opts$maxchunk) + 1

  if (er_opts$verbose) {
    message("extract_rast --> Extracting data from ", n_selbands,
            ifelse(date_check, " dates", "bands"),
            " - Please wait !")
    pb       <- utils::txtProgressBar(max = n_selbands, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts     <- list(progress = progress)
  } else {
    opts <- list()
  }

  #  ___________________________________________________________________________
  #  Extract data from in_rast - foreach cycle on selected bands of in_rast ####

  results <- foreach::foreach(
    band          = 1:n_selbands,
    .packages     = c("gdalUtils", "raster", "dplyr", "tibble",
                      "data.table", "sf", "velox"),
    .verbose      = verb_foreach,
    .options.snow = opts) %dopar%
    {
      # for (band in 1:1) {

      if (er_opts$verbose) {
        message("extract_rast--> Extracting data from ",
                ifelse(date_check, " dates", "bands"),
                " - Please wait !")
      }

      all_data     <- list()
      coords       <- list()
      stat_data    <- list()
      temp_outdata <- list()
      selband      <- seldates[band]
      chunk_n_all  <- 1 # Counter for non-empty chunks for all_data
      chunk_n_summ <- 1 # Counter for non-empty chunks for all_data
      start_cell   <- 1
      in_band      <- in_rast[[selbands[band]]]
      tempvrt      <- tempfile(fileext = ".vrt")

      if (in_band@file@name == "") {
        temprastfile <- tempfile(fileext = ".tif")
        writeRaster(in_band,
                    filename  = temprastfile,
                    options   = c("COMPRESS=DEFLATE", "PREDICTOR=3"),
                    overwrite = TRUE)
        in_band <- raster(temprastfile)
      }

      #TODO substitute with call to `crop_rast`
      buildvrt_string <- paste("-te ", paste(te, collapse = " "),
                               "-b ", band,
                               tempvrt,
                               in_band@file@name, " ")
      system2(file.path(find_gdal(), "gdalbuildvrt"),
              args = buildvrt_string, stdout = NULL)

      # "reload" in_band from the cropped vrt
      in_band    <- raster::raster(tempvrt)

      #TODO implement and test supersampling
      # ______________________________________________________________________
      # if "supersampling" requested, resample input raster to higher     ####
      # resolution (with nearest neaighbour)
      # if (supersample) {
      #
      #   temprast  <- tempfile(fileext = ".tif")
      #   raster::writeRaster(in_band, temprast)
      #   tempsuper <- tempfile(fileext = ".tif")
      #   in_band   <- gdalUtils::gdalwarp(
      #     temprast, tempsuper, tr = er_opts$rastres,
      #     te = raster::extent(in_band)[c(1, 3, 2, 4)], output_Raster = TRUE,
      #     multi = TRUE)
      #   in_band   <- velox::velox(in_band)
      #   in_band$crop(sp_polys)
      # }

#   ____________________________________________________________________________
#   if extraction needs to be done on chunks, identify the bboxes          ####
#   bboxes of each polygon in the input (to be able to check if all
#   pixels for that polygon has been already extracted)
#
      if (n_chunks > 1) {

        if (er_opts$verbose) {
          message("extract_rast --> Computing bounding boxes of input polygons")
        }

        bboxes <- in_vect_crop[c("mdxtnq", "geometry")] %>%
          data.table::data.table()
        bboxes <- bboxes[, list(min_y = sf::st_bbox(geometry)[2],
                                max_y = sf::st_bbox(geometry)[4]),
                        by = "mdxtnq"]
      } else {
        bboxes <- in_vect_crop[c("mdxtnq", "geometry")]
      }

      # -_______________________________________________________________________
      # Perform data extraction (in chunks if number of cells greater then ####
      # max_chunk)

      for (chunk in seq_len(n_chunks)) {

        if (er_opts$verbose) message("Working on chunk: ", chunk,
                                     " of: ", n_chunks, " of band: ", selband)

        # Identify row numbers of the current "chunk" ----
        startrow   <- ifelse(chunk == 1,
                             1,
                             1 + (chunk - 1) * ceiling(nrows / n_chunks))
        chunkrows  <- ifelse(chunk != n_chunks,
                             ceiling(nrows / n_chunks),
                             (1 + nrows - startrow))
        endrow      <- startrow + chunkrows - 1
        ncells      <- ncols * chunkrows
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

            if (er_opts$addgeom) {
              # Here we create a temporary "velox" raster, allowing to quickly
              # compute coordinates and save coordinates for the chunk in the
              # "coords" list

              temp_velox <- velox::velox(matrix(nrow = chunkrows, ncol = ncols),
                                         extent = as.numeric(ext_chunk),
                                         res = er_opts$rastres,
                                         crs = sp::proj4string(in_band))

              # Note: here `out_data$cell - start_cell` makes so that the first
              # cell of the chunk ends up in position 1 in the temporary velox
              # (otherwise, out_data$cell is > than the number of cells in
              # tempvelox, after the first chunk)

              coords <- temp_velox$getCoordinates()[(out_data$cell - start_cell),]  #nolint

              # add coordinates to the data table and remove the "cell" column
              out_data <- out_data[,c("cell", "x_coord", "y_coord") :=
                                     list(NULL, coords[,1], coords[,2])]

            } else {
              out_data <- out_data[, cell := NULL]
            }

            all_data[[chunk_n_all]] <- out_data
            chunk_n_all             <- chunk_n_all + 1

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

            if (n_chunks > 1) {
              complete_polys <- bboxes[min_y >= tot_ext_y$y_min]
            } else {
              complete_polys <- bboxes
            }

            if (length(complete_polys$mdxtnq) != 0) {

              data_for_summary  <- subset(temp_outdata, mdxtnq %in%
                                            unique(complete_polys$mdxtnq)) %>%
                data.table::setkey("mdxtnq")
              stat_data[[chunk_n_summ]] <- summarize_data(data_for_summary,
                                                          "mdxtnq",
                                                          er_opts$comp_quant,
                                                          er_opts$FUN,
                                                          selbands[band],
                                                          selband)

              temp_outdata  <- temp_outdata[!(mdxtnq %in%
                                                unique(complete_polys$mdxtnq))]

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

        all_data <- data.table::rbindlist(all_data) %>%
          data.table::setkey("mdxtnq")

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
          if (er_opts$addgeom) {
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
                                              "mdxtnq",
                                              er_opts$comp_quant,
                                              er_opts$FUN,
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

        all_data <- all_data[, c("band_n", "date", "N_PIX", "N") :=
                               list(band, seldates[band], .N, seq_len(.N)),
                             by = mdxtnq]
      }

      #  update progressbar
      if (er_opts$verbose) {
        Sys.sleep(0.001)
        utils::setTxtProgressBar(pb, band)
      }

      #   ______________________________________________________________________
      #   return data processed by the "worker" (a.k.a. the "band" results) ####

      out <- list(alldata = all_data, stats = stat_data)
      return(out)

    } # End Foreach cycle on bands

  parallel::stopCluster(cl)

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

    # if er_opts$addfeat, merge the extracted data with the missing shapefile
    # features
    if (er_opts$addfeat) {
      stat_data <- stat_data[{data.table::as.data.table(in_vect_crop) %>%
          setkey("mdxtnq")}]
    } else {
      if (!is.null(er_opts$id_field)) {

        stat_data <- stat_data[{
          data.table::as.data.table(
            in_vect_crop[,c(eval(er_opts$id_field), "mdxtnq")]
          ) %>%
            setkey("mdxtnq")
        }]
      }
    }

    # define the names and order of the output columns

    if (!is.null(er_opts$FUN)) {
      keep_cols <- c("mdxtnq", "band_n", "date",
                     "N_PIX", "myfun",
                     names_shp,
                     "geometry")
    } else {

      if (!er_opts$comp_quant) {
        keep_cols <- c("mdxtnq", "band_n", "date",
                       "N_PIX", "avg", "med", "sd", "min", "max",
                       names_shp,
                       "geometry")
      } else {
        keep_cols <- c("mdxtnq", "band_n", "date",
                       "N_PIX", "avg", "med", "sd", "min", "max",
                       "q01", "q05","q15", "q25", "q35", "q45", "q55", "q65",
                       "q75", "q85", "q95", "q99",
                       names_shp,
                       "geometry")
      }

    }

    if (!er_opts$addfeat) {
      if (is.null(er_opts$id_field)) {
        keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      } else {
        keep_cols <- keep_cols[which((!keep_cols %in% names_shp) &
                                       (keep_cols != er_opts$id_field))]
      }
    }

    # if addgeom is FALSE, remove geometry column
    if (!er_opts$addgeom) {

      keep_cols <- keep_cols[-length(keep_cols)]
      stat_data <- stat_data[, geometry := NULL]
    }

    if (!is.null(er_opts$id_field)) {
      stat_data <- stat_data[, mdxtnq := NULL]
      keep_cols <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }


    #   ________________________________________________________________________
    #   Build the final output and convert to tibble                        ####

    stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
      as_tibble()

    if (is.null(er_opts$id_field)) names(stat_data)[1] <- "id_feat"

    # If er_opts$addgeom, convert to a sf object - consider removing
    if (er_opts$addgeom) {
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

    # if er_opts$addfeat, merge the extracted data with the missing shapefile
    # features
    if (er_opts$addfeat) {
      all_data <- merge(all_data, in_vect_crop, by = "mdxtnq", all.y = TRUE)
    } else {
      if (!is.null(er_opts$id_field)) {

        all_data <- all_data[{
          data.table::as.data.table(
            in_vect_crop[,c(eval(er_opts$id_field), "mdxtnq")]) %>%
            setkey("mdxtnq")}]}
    }

    # define the order of the output columns
    keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX", "N",
                   "value",
                   names_shp,
                   "x_coord", "y_coord")

    if (!er_opts$addfeat) {
      if (is.null(er_opts$id_field)) {
        keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      } else {
        keep_cols <- keep_cols[which((!keep_cols %in% names_shp) &
                                       (keep_cols != er_opts$id_field))]
      }
    }
    # if addgeom is FALSE, remove coordinates
    if (!er_opts$addgeom) {
      keep_cols <- keep_cols[which(!keep_cols %in% c("x_coord", "y_coord"))]
    }
    if (!is.null(er_opts$id_field)) {
      keep_cols    <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }

    # build the final output and convert to tibble
    all_data  <- all_data[ , .SD, .SDcols = keep_cols] %>%
      tibble::as_tibble()

    # If er_opts$addgeom, convert to a sf object
    if (er_opts$addgeom) {
      all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"),
                                na.fail = FALSE)
      sf::st_set_crs(all_data, get_proj4string(in_rast))
    }
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
