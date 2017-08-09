#' @title er_polygons_velox
#' @description FUNCTION_DESCRIPTION
#' @param in_vect_zones PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param er_opts PARAM_DESCRIPTION
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

#'  \code{\link[gdalUtils]{gdalwarp}}

#'  \code{\link[sp]{proj4string}}

#'  \code{\link[velox]{velox}}

#'  \code{\link[utils]{setTxtProgressBar}},\code{\link[utils]{txtProgressBar}}
#' @rdname er_polygons
#' @export
#' @importFrom data.table data.table rbindlist setkey melt setcolorder
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr select group_by summarize
#' @importFrom foreach foreach %dopar%
#' @importFrom gdalUtils gdalwarp
#' @importFrom raster res writeRaster extent extract
#' @importFrom sf st_as_sf st_geometry st_set_crs
#' @importFrom sp proj4string
#' @importFrom velox velox
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom magrittr %>%

er_polygons_velox <- function(in_vect_zones, in_rast, seldates, selbands, n_selbands, date_check, er_opts) {

  #   ____________________________________________________________________________
  #   crop in_vect_zones to in_rast extent if necessary and identify "removed"   ####
  #   features + Find names of the attribute fields of the original shapefile

  if (er_opts$verbose) message("extract_rast --> Cropping the zones object on extent of the raster")
  #TODO  Do this only if extent of zone object is larger than that of the raster (in any direction)

  diff_ext <- as.numeric((extent(in_rast) + 461)[] ) -
    as.numeric(sf::st_bbox(in_vect_zones)[c(1,3,2,4)])

  if (max(abs(diff_ext) >= 1000)) {
    crop               <- er_crop_object(in_vect_zones, in_rast, er_opts$id_field, er_opts$verbose)
    in_vect_zones_crop <- crop$in_vect_zones_crop
    outside_feat       <- crop$outside_feat
  } else {
    in_vect_zones_crop <- in_vect_zones
    outside_feat       <- NULL
  }
  names_shp <- names(in_vect_zones_crop)[!names(in_vect_zones_crop) %in% c("mdxtnq", "geometry")]

  # Ientify the bboxes of polygons (required later to check if all data was loaded for a given
  # polygon when processing in "chunks")

  if (er_opts$verbose) message("extract_rast --> Computing bounding boxes of input polygons")

  #   ____________________________________________________________________________
  #   If `er_opts$rastres` is not null and is er_opts$smaller tha the original                ####
  #   resolution of in_rast, both `in_vect_zones` and `in_rast` are "super-sampled
  #   to `er_opts$rastres` resolution (using nn resampling) prior to data extraction

  supersample <- 0
  if (is.null(er_opts$rastres)) {
    er_opts$rastres = raster::res(in_rast)
  } else {
    if (length(er_opts$rastres) == 2 & length(is.finite(er_opts$rastres) == TRUE) & min(er_opts$rastres, na.rm = T) > 0) {
      er_opts$rastres     <- er_opts$rastres
      supersample <- 1
    } else {
      warning("extract_rast--> Provided `er_opts$rastres = `", er_opts$rastres, " seems invalid. It will be reset to `in_rast` resolution")
      er_opts$rastres = raster::res(in_rast)
    }
  }

  #   ____________________________________________________________________________
  #   Rasterize the shape to a temporary file (use `velox` to improve speed)  ####

  if (er_opts$verbose) {message("extract_rast--> Rasterizing shape")}

  # define the extent of the zone object
  zones_ext <- raster::extent(as.numeric(sf::st_bbox(in_vect_zones_crop))[c(1,3,2,4)])
  rastzone_object                  <- velox::velox(in_rast[[1]])
  rastzone_object$rasterbands[[1]] <- matrix(0,dim(in_rast)[1],dim(in_rast)[2])
  sp_polys <- as(in_vect_zones_crop, "Spatial")
  rastzone_object$rasterize(sp_polys, field = "mdxtnq", band = 1)
  rastzone_object$crop(zones_ext)
  # rastzone_object <- velox::velox(temp_rasterfile)

  #  ____________________________________________________________________________
  #  setup the processing: initialize variables and foreach loop            ####

  # Setup the number of cores: defaults to available cores - 2, but up to a maximum
  # of 8. If user-provided er_opts$ncores is greater than available cores - 2 or greater than 8
  # er_opts$ncores is re-set to the minimum between those two. If selbands < er_opts$ncores,
  # use only selbands cores

  if (is.null(er_opts$ncores)) {
    er_opts$ncores <- parallel::detectCores() - 2
  }
  er_opts$ncores <- min(c(er_opts$ncores, (parallel::detectCores() - 2)), 8)
  if (n_selbands < er_opts$ncores) (er_opts$ncores <- n_selbands)

  # cl      <- parallel::makeCluster(er_opts$ncores, outfile = "")
  cl        <- parallel::makeCluster(er_opts$ncores)
  doSNOW::registerDoSNOW(cl)

  # Initialize other variables and progress bar
  er_opts$maxchunk  <- er_opts$maxchunk/er_opts$ncores
  nrows             <- raster::nrow(in_vect_zones)
  ncols             <- raster::ncol(in_vect_zones)
  n_cells           <- nrows * ncols
  n_chunks          <- floor(n_cells / er_opts$maxchunk) + 1

  if (er_opts$verbose) {
    message("extract_rast--> Extracting data from ", n_selbands, ifelse(date_check, " dates", "bands"),
            " - Please wait !")
    pb       <- utils::txtProgressBar(max = n_selbands, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts     <- list(progress = progress)
  } else {
    opts = list()
  }

  #   _______________________________________________________________________________
  #   Extract data from in_rast - foreach cycle on selected bands of in_rast    ####

  results <- foreach::foreach(band = 1:n_selbands,
                              .packages = c("gdalUtils", "raster",
                                            "dplyr", "tibble",
                                            "data.table", "sf", "velox"),
                              .verbose = FALSE,
                              .options.snow = opts) %dopar%
                              {
                                # for (band in 1:1) {

                                if (er_opts$verbose) {
                                  message("extract_rast--> Extracting data from ", ifelse(date_check, " dates", "bands"), " - Please wait !")
                                }

                                all_data     <- list()
                                stat_data    <- list()
                                temp_outdata <- list()
                                selband      <- seldates[band]
                                chunk_n_all  <- 1 # Counter for non-empty chunks for all_data
                                chunk_n_summ <- 1 # Counter for non-empty chunks for all_data
                                start_cell   <- 1

                                #   ____________________________________________________________________________
                                #   Get current band as a `velox` object and crop it on polygons extent     ####

                                if (!supersample) {
                                  in_band    <- velox::velox(in_rast[[selbands[band]]])
                                  in_band$crop(sp_polys)
                                } else {
                                  # if "supersampling" requested, resample input raster to higher resolution ####
                                  # (with nearest neaighbour) TODO: verify if replacing with velox
                                  temprast  <- tempfile(fileext = ".tif")
                                  raster::writeRaster(in_band, temprast)
                                  tempsuper <- tempfile(fileext = ".tif")
                                  in_band   <- gdalUtils::gdalwarp(temprast, tempsuper, tr = er_opts$rastres,
                                                                   te = raster::extent(in_band)[c(1, 3, 2, 4)], output_Raster = TRUE ,
                                                                   multi = TRUE)
                                  in_band   <- velox::velox(in_band)
                                  in_band$crop(sp_polys)
                                }

                                #   _______________________________________________________________________________
                                #   Perform data extraction (in chunks if number of cells greater then max_chunk) ####

                                #for (chunk in seq_len(n_chunks)) {

                                if (er_opts$verbose) message("Working on chunk: ", chunk, " of: ", n_chunks, "of band: ", selband)

                                # Identify row numbers of the current "chunk" ----
                                # startrow   <- ifelse(chunk == 1, 1, 1 + (chunk - 1) * ceiling(nrows / n_chunks))
                                # chunkrows  <- ifelse(chunk != n_chunks,
                                #                      ceiling(nrows / n_chunks),
                                #                      (1 + nrows - startrow))
                                # endrow     <- startrow + chunkrows - 1
                                # ncells     <- ncols * chunkrows
                                # end_cell   <- start_cell + ncells - 1

                                #   ________________________________________________________________________
                                #   retrieve data of current "chunk" for the pixels included in the polygons ####
                                #   and put it in all_data[[chunk_n]] (if not null)

                                out_data  <- data.table::data.table(
                                  value   =  as.numeric(in_band$rasterbands[[1]]),
                                  cell    =  .N,
                                  mdxtnq  =  as.numeric(rastzone_object$rasterbands[[1]])
                                  , key = "mdxtnq") %>%
                                  subset(mdxtnq != 0)    %>%   # remove data outside polygons (== zone_id = 0 OR NA)
                                  subset(!is.na(mdxtnq))

                                # If out_data not empty (i.e., at least one pixel of current chunk beer_opts$longs to an
                                # polygon), put out_data in all_data[[chubnk_n_all]], then compute er_opts$summ_data
                                if (dim(out_data)[1] > 0) {
                                  if (er_opts$full_data) {
                                    all_data[[chunk_n_all]]  <- out_data
                                    # chunk_n_all              <- chunk_n_all + 1
                                  }
                                  if (er_opts$summ_data) {
                                    #   ____________________________________________________________________________
                                    #   verify if currently in out_data we have all the data corresponding   ####
                                    #   to any of the polygons. In that case, compute the summary statistics
                                    #   for those polygons, and remove their data from "out_data"
                                    # temp_outdata   <- data.table::rbindlist(list(temp_outdata,out_data))
                                    # check_n        <- temp_outdata %>%
                                    #   dplyr::group_by(mdxtnq) %>%
                                    #   dplyr::summarize(count = n())
                                    # codes_in_chunk <- unique(out_data$mdxtnq)
                                    # # polys_in_chunk <- subset(in_vect_zones_crop, mdxtnq %in% codes_in_chunk)
                                    # complete_polys <- NA
                                    # for (indcinc in seq_along(along.with = codes_in_chunk)) {
                                    #   cinc <- codes_in_chunk[indcinc]
                                    #   complete_polys[indcinc] <- (check_n[check_n$mdxtnq == cinc, 2] ==
                                    #                                 cells_per_poly[cells_per_poly$mdxtnq == cinc, 2])
                                    # }
                                    # #   ____________________________________________________________________________
                                    # #   If we have all data for any polygon, compute the summary statistics for ####
                                    # #   those, and remove them from "temp_outdata", then put temp_outdata in out_data
                                    # if (any(complete_polys == TRUE)) {

                                    # data_for_summary          <- temp_outdata
                                    stat_data                 <- fast_summ(out_data, "mdxtnq", er_opts$comp_quant, selbands[band], selband)
                                    # temp_outdata              <- subset(temp_outdata, !(mdxtnq %in% codes_in_chunk[complete_polys]))
                                    # chunk_n_summ              <- chunk_n_summ + 1
                                  }
                                }
                                # }
                                # start_cell  <- end_cell + 1  # Increment start_cell to get the start of the next chunk
                                # gc()
                                #} # end cycle on chunks

                                #   ____________________________________________________________________________
                                #   bind data from all chunks in `all_data`  and 'stat_data'                ####
                                # if (er_opts$full_data) {
                                #
                                #   all_data <- data.table::rbindlist(all_data) %>%
                                #     data.table::setkey("mdxtnq")
                                # }
                                #
                                # if (er_opts$summ_data) {
                                #   stat_data <- data.table::rbindlist(stat_data) %>%
                                #     data.table::setkey("mdxtnq")
                                #
                                # }

                                #   ____________________________________________________________________________
                                #   extract data for er_opts$small polygons if requested and necessary              ####

                                if (er_opts$small & length(unique(stat_data$mdxtnq) != length(unique(in_vect_zones_crop$mdxtnq)))) {

                                  miss_feat <- setdiff(unique(in_vect_zones_crop$mdxtnq), unique(stat_data$mdxtnq))
                                  for (mfeat in miss_feat) {

                                    poly_miss         <- sp_polys[sp_polys@data$mdxtnq == mfeat,]
                                    data_feat         <- raster::extract(in_rast[[band]], poly_miss, small = TRUE,
                                                                         method = "simple", df = TRUE, cellnumbers = TRUE)
                                    cell              <- data_feat[,2]
                                    miss_feat_data    <- data.table::data.table(value = data_feat[,3],
                                                                                cell =  cell,
                                                                                mdxtnq = mfeat)
                                    if (er_opts$full_data) {
                                      all_data        <- rbind(all_data, miss_feat_data)
                                    }
                                    # compute the summary statistics for the current "er_opts$small feature"
                                    if (er_opts$summ_data) {
                                      miss_feat_stats <- fast_summ(miss_feat_data, "mdxtnq", er_opts$comp_quant, selbands[band], selband)
                                      stat_data       <- rbind(stat_data, miss_feat_stats)
                                    }
                                  }
                                }

                                # ____________________________________________________________________________
                                # if er_opts$full_data required, extract all pixels values, and add some additional   ####
                                # column

                                if (er_opts$full_data) {
                                  all_data <- all_data[, list(band_n = selbands[band],
                                                              date   = seldates[band],
                                                              N_PIX  = seq(along.with = value),
                                                              value  = value,
                                                              cell   = cell
                                  ) , by = mdxtnq]
                                }

                                ##  ............................................................................
                                ##  update progressbar                                                      ####
                                if (er_opts$verbose) {
                                  Sys.sleep(0.001)
                                  utils::setTxtProgressBar(pb, band)
                                }

                                #   ____________________________________________________________________________
                                #   return data processed by the "worker" (a.k.a. the "band" results)       ####

                                out <- list(alldata = all_data, stats = stat_data)
                                return(out)

                              } # End Foreach cycle on bands

  parallel::stopCluster(cl)

  if (er_opts$verbose) message("extract_rast --> Data extraction completed. Building outputs")

  # ___________________________________________________________________________________
  # End of data loading from in_rast. Now in all_data/stat_data we have all values  ####
  # needed to build the output
  #

  # --------------------------------------------------------------------------------
  # if er_opts$keep_null selected and "outside features" found, replace
  # in_vect_zones_crop  with in_vect_zones, so that later joins "include" the missing
  # features

  if (er_opts$keep_null & !is.null(outside_feat)) {
    in_vect_zones_crop <- in_vect_zones
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the stats output list                       ####

  if (er_opts$summ_data) {

    # "bind" the different bands
    stat_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 2))) %>%
      data.table::setkey("mdxtnq")

    # if er_opts$addfeat, merge the extracted data with the shapefile features
    if (er_opts$addfeat) {
      stat_data <- stat_data[{
        data.table::as.data.table(in_vect_zones_crop) %>%
          setkey("mdxtnq")}
        ]
    }

    # define the order of the output columns

    if (!is.null(er_opts$FUN)) {
      keep_cols <- c("mdxtnq", "band_n", "date",
                     names_shp,
                     "N_PIX", "myfun",
                     "geometry")
    } else {

      if (!er_opts$comp_quant) {
        keep_cols <- c("mdxtnq", "band_n", "date",
                       names_shp,
                       "N_PIX", "avg", "med", "sd", "min", "max",
                       "geometry")
      } else {
        keep_cols <- c("mdxtnq", "band_n", "date",
                       names_shp,
                       "N_PIX", "avg", "med", "sd", "min", "max",
                       "q01", "q05","q15", "q25", "q35", "q45", "q55", "q65", "q75", "q85", "q95", "q99",
                       "geometry")
      }

    }
    if (!er_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]

    if (!er_opts$addgeom) {

      keep_cols <- keep_cols[-length(keep_cols)]
      stat_data <- stat_data[, geometry := NULL]
    }
    if (!is.null(er_opts$id_field)) {
      stat_data <- stat_data[, mdxtnq := NULL]
      keep_cols <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }

    # build the final output and convert to tibble
    stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
      as_tibble()

    if (is.null(er_opts$id_field)) names(stat_data)[1] = "id_feat"

    # If `er_opts$long`, reshape the stats output to a er_opts$long table format
    # (Consider removing)
    if (er_opts$long) {
      skip_cols <- ifelse(er_opts$comp_quant,18, 6)
      n_adds    <- length(names_shp)
      idcols    <- c(seq(1,length(stat_data) - skip_cols - 1), length(stat_data))
      stat_data <- data.table::melt(stat_data, idcols)
      keep_cols <- c("id_feat",
                     "band_n", "date",
                     names_shp,
                     "variable", "value",
                     "geometry")
      if (!er_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      if (!er_opts$addgeom) keep_cols <- keep_cols[-length(keep_cols)]
      if (!is.null(er_opts$id_field)) {
        keep_cols    <- keep_cols[which(keep_cols != er_opts$id_field)]
        keep_cols[1] <- eval(er_opts$id_field)
      }
      stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
        tibble::as_tibble()
    }
    # If er_opts$addgeom, convert to a sf object
    if (er_opts$addgeom) {
      stat_data <- sf::st_as_sf(stat_data)
    }
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the alldata list (includes adding the "point"
  #   coordinates and transforming to a `sf` object                           ####

  if (er_opts$full_data) {
    # "bind" the different bands
    all_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 1))) %>%
      data.table::setkey("mdxtnq")
    sf::st_geometry(in_vect_zones_crop) <- NULL

    # if er_opts$addfeat, merge the extracted data with the shapefile features
    if (er_opts$addfeat) {
      all_data <- merge(all_data, in_vect_zones_crop, by = "mdxtnq", all.y = TRUE)
    }

    # define the order of the output columns
    keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX", "N",
                   names_shp,
                   "value",
                   "x_coord", "y_coord")
    if (!er_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    if (!er_opts$addgeom) keep_cols <- keep_cols[which(!keep_cols %in% c("x_coord", "y_coord"))]
    if (!is.null(er_opts$id_field)) {
      keep_cols    <- keep_cols[which(keep_cols != er_opts$id_field)]
      keep_cols[1] <- eval(er_opts$id_field)
    }

    # build the final output and convert to tibble
    all_data  <- all_data[ , .SD, .SDcols = keep_cols] %>%
      tibble::as_tibble()

    # If er_opts$addgeom, convert to a sf object
    if (er_opts$addgeom) {
      all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"), na.fail = FALSE) %>%
        sf::st_set_crs(sp::proj4string(in_rast))
    }
  }

  #   ____________________________________________________________________________
  #   Final cleanup                                                           ####

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
