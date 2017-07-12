#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param zone_object PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param rastres PARAM_DESCRIPTION
#' @param maxchunk PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param id_field PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param long PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION
#' @param comp_quant PARAM_DESCRIPTION
#' @param small PARAM_DESCRIPTION
#' @param addfeat PARAM_DESCRIPTION
#' @param addgeom PARAM_DESCRIPTION
#' @param keep_null PARAM_DESCRIPTION
#' @param zone_type PARAM_DESCRIPTION
#' @param summ_data PARAM_DESCRIPTION
#' @param full_data PARAM_DESCRIPTION
#' @param ncores PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[doSNOW]{registerDoSNOW}}

#'  \code{\link[foreach]{foreach}}

#'  \code{\link[gdalUtils]{gdal_rasterize}},\code{\link[gdalUtils]{gdalwarp}}

#'  \code{\link[sp]{proj4string}}

#'  \code{\link[tibble]{as_tibble}}

#'  \code{\link[tidyr]{gather}}

#'  \code{\link[utils]{txtProgressBar}},\code{\link[utils]{setTxtProgressBar}}
#' @rdname er_poly_parallel
#' @export
#' @importFrom data.table data.table rbindlist setkey setcolorder
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr case_when group_by summarize mutate select right_join
#' @importFrom foreach foreach %dopar%
#' @importFrom gdalUtils gdal_rasterize gdalwarp
#' @importFrom raster res crop extent writeRaster getValues extract xyFromCell
#' @importFrom sf st_bbox st_as_sf st_geometry st_crs
#' @importFrom sp proj4string
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom utils txtProgressBar setTxtProgressBar


er_poly_parallel <- function(zone_object,
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
                             full_data,
                             ncores) {


  #   ____________________________________________________________________________
  #   crop zone_object to in_rast extent if necessary and identify "removed"   ####
  #   features
  #
  crop             <- er_crop_object(zone_object, in_rast, id_field)
  zone_object_crop <- crop$zone_object_crop
  outside_feat     <- crop$outside_feat
  # Find names of the attribute fields of the original shapefile
  names_shp  <- names(zone_object_crop)[!names(zone_object_crop) %in% c("mdxtnq", "geometry")]

  #   ____________________________________________________________________________
  #   If zone_object is not already a raster file, then rasterize the polygon  ####
  #   shapefile. (If `rastres` is not null and is smaller tha the original
  #   resolution of in_rast, both `zone_object` and `in_rast` are "super-sampled
  #   to `rastres` resolution (using nn resampling) prior to data extraction
  #


  supersample <- 0
  if (is.null(rastres)) {
    rastres = raster::res(in_rast)
  } else {
    if (length(rastres) == 2 & length(is.finite(rastres) == TRUE) & min(rastres, na.rm = T) > 0) {
      rastres     <- rastres
      supersample <- 1
    } else {
      warning("extract_rast--> Provided `rastres = `", rastres, " seems invalid. It will be reset to `in_rast` resolution")
      rastres = raster::res(in_rast)
    }
  }

  #   ____________________________________________________________________________
  #   Rasterize the shape to a temporary file                                 ####

  if (verbose) {message("extract_rast--> Rasterizing shape")}
  if (verbose) {message("extract_rast--> Writing temporary shapefile")}
  # temp_shapefile <- tempfile(tmpdir = tempdir(), fileext = ".shp")
  # writeshape(zone_object_crop, temp_shapefile, overwrite = TRUE)
  # if (verbose) {(message("extract_rast--> Writing temporary rasterized shapefile"))}
  # temp_rasterfile = tempfile(tmpdir = tempdir(), fileext = ".tiff")
  # max_id <- max(zone_object_crop$mdxtnq)
  # ot <- dplyr::case_when(
  #   (max_id <= 255) == 1 ~ "Byte",
  #   (max_id >= 255 & max_id < 65535) == 1 ~ "Int16",
  #   (max_id >= 65536) == 1 ~ "Int32"
  # )
  # zone_raster <- gdalUtils::gdal_rasterize(temp_shapefile,
  #                                          temp_rasterfile,
  #                                          tr  = rastres,
  #                                          te  = extent(in_rast[[1]])[c(1,3,2,4)],
  #                                          a   = "mdxtnq",
  #                                          ot  = ot,
  #                                          tap = T,
  #                                          output_Raster = TRUE)
  # rastzone_object <- crop(zone_raster, polys)
  sp_polys <- as(zone_object_crop, "Spatial")
  rastzone_object <- velox::velox(in_rast[[1]])
  rastzone_object$rasterbands[[1]] <- matrix(0,dim(in_rast)[1],dim(in_rast)[2])
  rastzone_object$rasterize(sp_polys, field = "mdxtnq", band = 1)
  rastzone_object$crop(sp_polys)
  # plot(vx$as.RasterLayer())

  #  ____________________________________________________________________________
  #  setup thew processing: initialize variables and foreach loop            ####

  # Setup the number of cores: defaults to available cores - 2, but up to a maximum
  # of 8. If user-provided ncores is greter than available cores - 2 or greater than 8
  # ncores is re-set to the minimum between those two
  if (is.null(ncores)) {
    ncores <- parallel::detectCores() - 2
  }
  ncores <- min(c(ncores, (parallel::detectCores() - 2)), 8)

  # if number of bands to be processed is 1, use only one core. If number of bands < ncores,
  # use only n_selbands cores
  if (n_selbands < ncores) ncores <- n_selbands

  # cl      <- parallel::makeCluster(ncores, outfile = "")
  cl        <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cl)
  maxchunk  <- maxchunk/ncores

  n_cells   <- prod(rastzone_object$dim)
  nrows     <- rastzone_object$dim[1]
  ncols     <- rastzone_object$dim[2]
  n_chunks  <- floor(n_cells / maxchunk) + 1
  stat_data <- list()
  all_data  <- list()

  cells_per_poly <- rastzone_object$extract(sp_polys, fun = length) %>%
    data.frame(mdxtnq = 1:length(.), count = (.)) %>%
    dplyr::select(2,3)

  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  if (verbose) {
    pb <- txtProgressBar(max = n_selbands, style = 3)
    progress  <- function(n) utils::setTxtProgressBar(pb, n)
    message("extract_rast--> Extracting data from ", n_selbands, ifelse(date_check, " dates", "bands"),
            " - Please wait !")
    pb <- utils::txtProgressBar(max = n_selbands, style = 3)

  }

  #   ____________________________________________________________________________
  #   Extract data from in_rast - foreach cycle on selected bands of in_rast    ####
  #
  results <- foreach::foreach(band = 1:n_selbands, .packages = c("gdalUtils", "raster", "dplyr", "tibble", "data.table", "sf"),
                              .verbose = FALSE, .options.snow = opts) %dopar%
        {
          # for (band in 1:2) {
          if (verbose) {
            message("extract_rast--> Extracting data from ", ifelse(date_check, " dates", "bands"), " - Please wait !")
          }


          all_data     <- list()
          stat_data    <- list()
          selband      <- seldates[band]
          chunk_n_all  <- 1 # Counter for non-empty chunks for all_data
          chunk_n_summ <- 1 # Counter for non-empty chunks for all_data
          start_cell   <- 1
          temp_outdata <- list()

          in_band      <- velox::velox(in_rast[[selbands[band]]])
          in_band$crop(sp_polys)

          #   ____________________________________________________________________________
          #   if "supersampling" requested, resample input raster to higher resolution ####
          #   (with nearest neaighbour)

          if (supersample) {
            temprast  <- tempfile(fileext = ".tif")
            raster::writeRaster(in_band, temprast)
            tempsuper <- tempfile(fileext = ".tif")
            in_band   <- gdalUtils::gdalwarp(temprast, tempsuper, tr = rastres,
                                             te = raster::extent(in_band)[c(1, 3, 2, 4)], output_Raster = TRUE ,
                                             multi = TRUE)
          }

          #   _______________________________________________________________________________
          #   Perform data extraction in chunks if number of cells greater then max_chunk ####


          for (chunk in seq_len(n_chunks)) {

            if (verbose) message("Working on chunk: ", chunk, " of: ", n_chunks, "of band: ", selband)
            # Identify row numbers of the "chunk" ----

            startrow   <- ifelse(chunk == 1, 1, 1 + (chunk - 1) * ceiling(nrows / n_chunks))
            chunkrows  <- ifelse(chunk != n_chunks,
                                 ceiling(nrows / n_chunks),
                                 (1 + nrows - startrow))
            endrow     <- startrow + chunkrows - 1
            ncells     <- ncols * chunkrows
            end_cell   <- start_cell + ncells - 1

            #   ________________________________________________________________________
            #   retrieve data of current "chunk" for the pixels included in the polygons ####
            #   and put it in all_data[[chunk_n]] (if not null)

            out_data  <- data.table::data.table(
              value   =  as.numeric(in_band$rasterbands[[1]][start_cell:end_cell]),
              cell    =  seq(start_cell,end_cell),
              mdxtnq  =  as.numeric(rastzone_object$rasterbands[[1]][start_cell:end_cell])
              , key = "mdxtnq") %>%
              subset(mdxtnq != 0)    %>%   # remove data outside polygons (== zone_id = 0 OR NA)
              subset(!is.na(mdxtnq))

            if (dim(out_data)[1] > 0) {
              if (full_data) {
                all_data[[chunk_n_all]]  <- out_data
                chunk_n_all <- chunk_n_all + 1
              }

              if (summ_data) {
                #   ____________________________________________________________________________
                #   verify if currently in out_data we have all the "lines" corresponding   ####
                #   to any of the polygons. In that case, compute the summary statistics
                #   for those polygons, and remove their data from "out_data"
                temp_outdata <- rbindlist(list(temp_outdata,out_data))
                check_n <- temp_outdata %>%
                  dplyr::group_by(mdxtnq) %>%
                  dplyr::summarize(count = n())
                codes_in_chunk <- unique(out_data$mdxtnq)
                polys_in_chunk <- subset(zone_object_crop, mdxtnq %in% codes_in_chunk)
                complete_polys <- NA
                for (indcinc in seq_along(along.with = codes_in_chunk)) {
                  cinc <- codes_in_chunk[indcinc]
                  complete_polys[indcinc] <- (check_n[check_n$mdxtnq == cinc, 2] == cells_per_poly[cells_per_poly$mdxtnq == cinc, 2])
                }

                #   ____________________________________________________________________________
                #   If we have all data for any polygon, compute the summary statistics for ####
                #   those, and remove them from "temp_outdata", then put temp_outdata in out_data
                if (any(complete_polys == TRUE)) {

                  data_for_summary          <- subset(temp_outdata, mdxtnq %in% codes_in_chunk[complete_polys])
                  stat_data[[chunk_n_summ]] <- fast_summ(data_for_summary, "mdxtnq", comp_quant, selbands[band], selband)
                  temp_outdata              <- subset(temp_outdata, !(mdxtnq %in% codes_in_chunk[complete_polys]))
                  chunk_n_summ              <- chunk_n_summ + 1
                }
              }

            }
            # browser()
            start_cell  <- end_cell + 1
            gc()
          }

          #   ____________________________________________________________________________
          #   Join all chunks in `all_data`  and 'stat_data'                          ####
          if (full_data) {

            all_data <- data.table::rbindlist(all_data) %>%
              data.table::setkey("mdxtnq")
          }

          if (summ_data) {
            stat_data <- data.table::rbindlist(stat_data) %>%
              data.table::setkey("mdxtnq")

          }

          # } else {
          #   ____________________________________________________________________________
          #   # if chunking not needed, load the full table of data in `all_data`     ####
          # codes <- data.table(mdxtnq = raster::getValues(rastzone_object))
          # names(codes) = "mdxtnq"
          # setkey(codes, "mdxtnq")


          # # cell_coords <- raster::xyFromCell(in_band, 1:raster::ncell(in_band))
          # values      <- raster::getValues(in_band)
          # all_data    <- data.table::data.table(
          #   value   =  as.numeric(values),
          #   cell    = seq(1, ncell(in_band), 1),
          #   mdxtnq  = as.numeric(raster::getValues(rastzone_object))
          #   , key = "mdxtnq") %>%
          #   subset(mdxtnq != 0) %>%
          #   subset(!is.na(mdxtnq)) # remove data outside polygons (== zone_id = 0 OR NA)
          #
          #   ____________________________________________________________________________
          #   extract data for small polygons if requested                            ####

          if (small & length(unique(all_data$mdxtnq) != length(unique(zone_object_crop$mdxtnq)))) {
            # browser()
            miss_feat <- setdiff(unique(zone_object_crop$mdxtnq), unique(all_data$mdxtnq))
            for (mfeat in miss_feat) {
              poly_miss         <- as(zone_object_crop[mfeat,], "Spatial")
              data_feat         <- raster::extract(in_rast[[band]], poly_miss, small = T, method = "simple",
                                                   df = TRUE, cellnumbers = T)
              cell              <- data_feat[,2]
              miss_feat_data    <- data.table(value = data_feat[,3],
                                           cell =  cell,
                                           mdxtnq = mfeat)
              all_data          <- rbind(all_data, miss_feat_data)
              if (summ_data) {
                miss_feat_stats <- fast_summ(miss_feat_data, "mdxtnq", comp_quant, selbands[band], selband)
                stat_data       <- rbind(stat_data, miss_feat_stats)
              }
            }

          }

          # _______________________________________________________________________________
          # if summ_data selected, compute the statistics and set them in stat_data
          # if (summ_data) {
          #   stat_data <- fast_summ(all_data, "mdxtnq", comp_quant, selbands[band], selband)
          # }

          # }


          # #################################################################################
          # End of data loading from in_rast[band].
          # #################################################################################

          # ____________________________________________________________________________
          # if full_data required, extract all pixels values, add some additional
          # column and convert to tibble                                             ####

          if (full_data) {
            # browser()
            all_data <- all_data[, list(band_n = band ,
                                        date = seldates[band],
                                   N_PIX = seq(along = value),
                                   value = value,
                                   cell = cell
            ) , by = mdxtnq]
          }
          # browser()
          ##  ............................................................................
          ##  update progressbar                                                      ####
          if (verbose) {
            Sys.sleep(0.001)
            utils::setTxtProgressBar(pb, band)
          }
          ##  ............................................................................
          ##  return data processed by the "worker" (a.k.a. the "band" results)       ####

          out <- list(ts_full = all_data, ts_summ = stat_data)
          return(out)
        }

  parallel::stopCluster(cl)
  # browser()
  if (verbose) message("extract_rast--> building outputs")
  # ___________________________________________________________________________________
  # End of data loading from in_rast. Now in all_data/stat_data we have all values  ####
  # needed to build the output
  # #################################################################################

  # --------------------------------------------------------------------------------
  # if keep_null selected and "outside features found, replace zone_object_crop ####
  # with zone_object, so that later joins "include" the missing features

  if (keep_null & !is.null(outside_feat)) {
    zone_object_crop <- zone_object
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the ts_summ list                              ####
  # browser()
  if (summ_data) {

    # "bind" the different bands
    stat_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 2)))
    if (addfeat) {
      stat_data <- merge(stat_data, zone_object_crop, by = "mdxtnq", all.y = TRUE)
    }
    if (!comp_quant) {
      keep_cols <- c("mdxtnq", "band_n", "date",
                     names_shp,
                     "N", "avg", "med", "sd", "min", "max",
                     "geometry")
    } else {
      keep_cols <- c("mdxtnq", "band_n", "date",
                     names_shp,
                     "N", "avg", "med", "sd", "min", "max",
                     "q01", "q05","q15", "q25", "q35", "q45", "q55", "q65", "q75", "q85", "q95", "q99",
                     "geometry")
    }
    if (!addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    if (!addgeom) keep_cols <- keep_cols[-length(keep_cols)]
    # browser()
    # if `id_field` is missing or not valid, create a new column named "id_feat" and place it in
    # first place
    if (!is.null(id_field)) {

      stat_data <- stat_data[, mdxtnq := NULL]
      keep_cols <- keep_cols[which(keep_cols != id_field)]
      keep_cols[1] <- eval(id_field)
      # browser()
      # names_tmp <- names(stat_data)

      # stat_data2 <- dplyr::right_join(ssetkeyvtat_data, zone_object_crop, by = "mdxtnq") %>%
      #   tibble::as_tibble()







      # # If addfeat == FALSE, remove the columns present only in the shapefile
      # if (!addfeat) {
      #   stat_data <- dplyr::select(stat_data, which(names(stat_data) %in% names_tmp)) %>%
      #     tibble::as_tibble()
      # }
    }

    stat_data <- setcolorder(stat_data, keep_cols)
    if (is.null(id_field)) names(stat_data)[1] = "id_feat"
    # } else {
    #   stat_data <- stat_data[, mdxtnq := NULL]
    #   if (!comp_quant) {
    #     keep_cols <- c(eval(id_field), "band_n", "date",
    #                    names_shp,
    #                    "N", "avg", "med", "sd", "min", "max",
    #                    "geometry")
    #   } else {
    #     keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX",
    #                    names_shp,
    #                    "N", "avg", "med", "sd", "min", "max",
    #                    "q01", "q05","q15", "q25", "q35", "q45", "q55", "q65", "q75", "q85", "q95", "q99",
    #                    "geometry")
    #   }
    #   if (!addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    #   if (!addgeom) keep_cols <- keep_cols[-length(keep_cols)]
    #
    #   stat_data <- setcolorder(stat_data, keep_cols)
    #
    #   # if `id_field` is valid, find it and put it in first place in the output
    #   # first place
    #   # browser()
    #   stat_data <- stat_data %>%
    #     dplyr::right_join(zone_object_crop, by = "mdxtnq") %>%
    #     tibble::as_tibble() %>%
    #     dplyr::select(-mdxtnq)
    #
    #   where_id <- which(names(stat_data) == id_field)
    #   ncolumns <- length(stat_data)
    #
    #   if (where_id == ncolumns - 1) {
    #     if (!comp_quant) {
    #       col_order = c(where_id, 1:2, 3:8, ncolumns)}
    #     else {
    #       col_order = c(where_id, 1:2, 3:18, ncolumns)
    #     }
    #   } else {
    #     if (!comp_quant) {
    #       if (where_id == 9) {
    #         col_order <- c(9, 1:2, 10:(ncolumns - 1), 3:8, ncolumns)
    #       } else {
    #         col_order <- c(where_id, 1:2, 9:(where_id - 1), (where_id + 1):(ncolumns - 1), 3:8, ncolumns)
    #       }
    #     } else {
    #       if (where_id == 19) {
    #         col_order <- c(19, 1:2, 20:(ncolumns - 1), 3:18, ncolumns)
    #       } else {
    #         col_order <- c(where_id, 1:2, 19:(where_id - 1), (where_id + 1):(ncolumns - 1), 3:18, ncolumns)
    #       }
    #     }
    #   }
    #   browser()
    #   stat_data <- stat_data %>%
    #     dplyr::select(col_order)
    #
    #   # If addfeat == FALSE, remove the columns present only in the shapefile
    #   if (!addfeat) {
    #     names_shp  <- names(zone_object_crop)[names(zone_object_crop) != "mdxtnq" &
    #                                             names(zone_object_crop) != id_field ]
    #     keep_names <- which(names(stat_data) %in% setdiff(names(stat_data), names_shp))
    #
    #     stat_data  <- dplyr::select(stat_data, keep_names)

    #   }
    # }
    #
    #   ____________________________________________________________________________
    #   If `long`, reshape the ts_summ output to a long table                   ####

    # data.table::setkeyv(stat_data,colnames(stat_data)[c(1,2)])

    if (long) {

      skip_cols <- ifelse(comp_quant,18, 6)
      n_adds    <- length(names_shp)
      idcols    <- c(seq(1,length(stat_data) - skip_cols - 1), length(stat_data))
      stat_data <- data.table::melt(stat_data, idcols)
      keep_cols <- c("id_feat",
                     "band_n", "date",
                     names_shp,
                     "variable", "value",
                     "geometry")
      if (!addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      if (!addgeom) keep_cols <- keep_cols[-length(keep_cols)]
      if (!is.null(id_field)) {
        keep_cols    <- keep_cols[which(keep_cols != id_field)]
        keep_cols[1] <- eval(id_field)
      }
      stat_data <- setcolorder(stat_data, keep_cols) %>%
        as_tibble()
      # stat_data2 <- stat_data %>%
      #   tidyr::gather(variable, value, -idcols) %>%
      #   dplyr::select(1:(3 + n_adds),
      #                 ((length(.) - 1):length(.)),
      #                 (length(.) - 2)) %>%
      #   dplyr::mutate(variable = as.factor(variable)) %>%
      #   st_as_sf()


      # data.table::setkeyv(stat_data,colnames(stat_data)[c(1,2)]) %>%
      #   as_tibble()
    }

    if (addgeom) {
      stat_data <- st_as_sf(stat_data)
    }
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the ts_full list (includes adding the "point"
  #   coordinates and transforming to a `sf` object                           ####

  if (full_data) {

    # "bind" the different bands
    all_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 1))) %>%
      setkey("mdxtnq")
    sf::st_geometry(zone_object_crop) <- NULL
    if (addfeat) {
      all_data <- merge(all_data, zone_object_crop, by = "mdxtnq", all.y = TRUE)
    }
    # if `id_field` is missing or not valid, create a new column named "id_feat" and place it in
    # first place
    infast    <- velox(in_rast[[1]])
    coords    <- infast$getCoordinates()[all_data$cell,]
    all_data  <- all_data[,c("x_coord", "y_coord") := list(coords[,1], coords[,2])]

    all_data  <- all_data[, cell := NULL]

    keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX",
                   names_shp,
                   "value",
                   "x_coord", "y_coord")
    if (!addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    # if (!addgeom) keep_cols <- keep_cols[-length(keep_cols)]

    if (!is.null(id_field)) {
      keep_cols    <- keep_cols[which(keep_cols != id_field)]
      keep_cols[1] <- eval(id_field)
    }

    # if (!is.null(id_field)) {
    #   names_tmp <- names(all_data)
    #   # If addfeat == FALSE, remove the columns present only in the shapefile
    #   if (!addfeat) {
    #     keep_cols <- which(names(all_data) %in% names_tmp)
    #     all_data  <- all_data[ , .SD, .SDcols = keep_cols]
    #   }
    #   names(all_data)[1] = "id_feat"
    # } else {
    #
    #   # if `id_field` is valid, find it and put it in first place in the output
    #   # first place, then reorder other columns
    #   keep_cols <- c(eval(id_field), "band_n", "date", "N_PIX",
    #                  names(zone_object_crop)[which(!names(zone_object_crop) %in% c(eval(id_field), "mdxtnq"))],
    #                  "value",
    #                  "x_coord", "y_coord")
    #   #
    #   #             if (dim(all_data)[2] == ncolumns) {
    #   #         col_order <- c(where_id,
    #   #                        2:6) - 1
    #   #       } else {
    #   #         col_order <- c(where_id,
    #   #                        2:3,
    #   #                        if (where_id == 7) {seq(8,ncolumns)} else {
    #   #                          c(seq(7,(where_id - 1)), seq((where_id + 1), ncolumns))},
    #   #                        # if (where_id == ncolumns) {ncolumns} else {seq((where_id + 1), ncolumns)},
    #   #                        4:6) - 1
    #   #

    all_data  <- all_data[ , .SD, .SDcols = keep_cols]
    # data.table::setkeyv(all_data, colnames(all_data)[c(1,2,4)])
    all_data <- tibble::as_tibble(all_data)
      # if (addgeom) {
      #   all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"), na.fail = FALSE)
      # }


    #   # If addfeat == FALSE, remove the columns present only in the shapefile
    #   if (!addfeat) {
    #     names_shp  <- names(zone_object_crop)[names(zone_object_crop) != "mdxtnq" &
    #                                             names(zone_object_crop) != id_field ]
    #     keep_names <- which(names(all_data) %in% setdiff(names(all_data), names_shp))
    #     all_data   <- dplyr::select(all_data, keep_names)
    #   }
    # }

    # Add a unique identifier per pixel
    # all_data <- all_data[,x := Reduce(function(...) paste(..., sep = "_"), .SD[, c(1,4)])]
    if (addgeom) {
      # browser()
      all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"), na.fail = FALSE)
    }
    # set projection of the output
    if(addgeom) {
      sf::st_crs(all_data) <- sp::proj4string(in_rast)
    }
  }

  # if dates were not passed, then change the name of column 3 to "band_name"
  if (!date_check) {
    if (summ_data) names(stat_data)[3] <- "band_name"
    if (full_data) names(all_data)[3]  <- "band_name"
  }

  if (!full_data) all_data <- NULL
  if (!summ_data) stat_data <- NULL

  # create the output list
  ts_out <- list(ts_summ = stat_data, ts_full = all_data)

  # Cleanup temporary files and do a last gc()
  # if (file.exists(temp_rasterfile)) file.remove(temp_rasterfile)
  # if (file.exists(temp_shapefile)) file.remove(temp_shapefile)
  gc()

  return(ts_out)
}
