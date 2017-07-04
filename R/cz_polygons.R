#' @title cz_polygons
#' @description FUNCTION_DESCRIPTION
#' @param in_vect_zones PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param seldates PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION
#' @param n_selbands PARAM_DESCRIPTION
#' @param date_check PARAM_DESCRIPTION
#' @param cz_opts PARAM_DESCRIPTION
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
#' @rdname cz_polygons
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

cz_polygons <- function(in_vect_zones, in_rast, seldates, selbands, n_selbands, date_check, cz_opts) {

  #   ____________________________________________________________________________
  #   crop in_vect_zones to in_rast extent if necessary and identify "removed"   ####
  #   features + Find names of the attribute fields of the original shapefile
  #
  crop               <- cz_crop_object(in_vect_zones, in_rast, cz_opts$id_field, cz_opts$verbose)
  in_vect_zones_crop <- crop$in_vect_zones_crop
  outside_feat       <- crop$outside_feat
  names_shp          <- names(in_vect_zones_crop)[!names(in_vect_zones_crop) %in% c("mdxtnq", "geometry")]

  #   ____________________________________________________________________________
  #   If `cz_opts$rastres` is not null and is cz_opts$smaller tha the original                ####
  #   resolution of in_rast, both `in_vect_zones` and `in_rast` are "super-sampled
  #   to `cz_opts$rastres` resolution (using nn resampling) prior to data extraction

  supersample <- 0
  if (is.null(cz_opts$rastres)) {
    cz_opts$rastres = raster::res(in_rast)
  } else {
    if (length(cz_opts$rastres) == 2 & length(is.finite(cz_opts$rastres) == TRUE) & min(cz_opts$rastres, na.rm = T) > 0) {
      cz_opts$rastres     <- cz_opts$rastres
      supersample <- 1
    } else {
      warning("comp_zonal--> Provided `cz_opts$rastres = `", cz_opts$rastres, " seems invalid. It will be reset to `in_rast` resolution")
      cz_opts$rastres = raster::res(in_rast)
    }
  }

  #   ____________________________________________________________________________
  #   Rasterize the shape to a temporary file (use `velox` to improve speed)  ####

  if (cz_opts$verbose) {message("comp_zonal--> Rasterizing shape")}

  sp_polys                         <- as(in_vect_zones_crop, "Spatial")
  rastin_vect_zones <- velox::velox(matrix(data = 0.0 ,
                                         nrow = dim(in_rast)[1],
                                         ncol = dim(in_rast)[2]),
                                  extent = raster::extent(in_rast[[1]]),
                                  res = raster::res(in_rast[[1]]),
                                  crs = sp::proj4string(in_rast[[1]]))
  rastin_vect_zones$crop(sp_polys)
  rastin_vect_zones$rasterize(sp_polys, field = "mdxtnq", band = 1)


  #   ____________________________________________________________________________
  #   Compute the number of pixels included in each polygon                   ####
  #   This is used later to verify if, on chunked processing, all data from
  #   a polygon were already retrieved
  #
  cells_per_poly <- rastin_vect_zones$extract(sp_polys, fun = length) %>%
    data.frame(mdxtnq = 1:length(.), count = (.)) %>%
    dplyr::select(2,3)


  #  ____________________________________________________________________________
  #  setup the processing: initialize variables and foreach loop            ####

  # Setup the number of cores: defaults to available cores - 2, but up to a maximum
  # of 8. If user-provided cz_opts$ncores is greter than available cores - 2 or greater than 8
  # cz_opts$ncores is re-set to the minimum between those two. If selbands < cz_opts$ncores,
  # use only selbands cores

  if (is.null(cz_opts$ncores)) {
    cz_opts$ncores <- parallel::detectCores() - 2
  }
  cz_opts$ncores <- min(c(cz_opts$ncores, (parallel::detectCores() - 2)), 8)
  if (n_selbands < cz_opts$ncores) (cz_opts$ncores <- n_selbands)

  # cl      <- parallel::makeCluster(cz_opts$ncores, outfile = "")
  cl        <- parallel::makeCluster(cz_opts$ncores)
  doSNOW::registerDoSNOW(cl)

  # Initialize other variables and progress bar
  cz_opts$maxchunk  <- cz_opts$maxchunk/cz_opts$ncores
  n_cells           <- prod(rastin_vect_zones$dim)
  nrows             <- rastin_vect_zones$dim[1]
  ncols             <- rastin_vect_zones$dim[2]
  n_chunks          <- floor(n_cells / cz_opts$maxchunk) + 1

  if (cz_opts$verbose) {
    message("comp_zonal--> Extracting data from ", n_selbands, ifelse(date_check, " dates", "bands"),
            " - Please wait !")
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    pb       <- utils::txtProgressBar(max = n_selbands, style = 3)
    opts     <- list(progress = progress)
  } else {
    opts = list()
  }

  # if(cz_opts$verbose)
  # progress          <- function(n) utils::setTxtProgressBar(pb, n)
  #   _______________________________________________________________________________
  #   Extract data from in_rast - foreach cycle on selected bands of in_rast    ####

  results <- foreach::foreach(band = 1:n_selbands, .packages = c("gdalUtils", "raster", "dplyr", "tibble",
                                                                 "data.table", "sf", "velox"), .verbose = FALSE,
                              .options.snow = opts) %dopar%
    {
      # for (band in 1:1) {

      if (cz_opts$verbose) {
        message("comp_zonal--> Extracting data from ", ifelse(date_check, " dates", "bands"), " - Please wait !")
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
        in_band   <- gdalUtils::gdalwarp(temprast, tempsuper, tr = cz_opts$rastres,
                                         te = raster::extent(in_band)[c(1, 3, 2, 4)], output_Raster = TRUE ,
                                         multi = TRUE)
        in_band   <- velox::velox(in_band)
        in_band$crop(sp_polys)
      }

      #   _______________________________________________________________________________
      #   Perform data extraction (in chunks if number of cells greater then max_chunk) ####

      for (chunk in seq_len(n_chunks)) {


        if (cz_opts$verbose) message("Working on chunk: ", chunk, " of: ", n_chunks, "of band: ", selband)

        # Identify row numbers of the current "chunk" ----
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
          mdxtnq  =  as.numeric(rastin_vect_zones$rasterbands[[1]][start_cell:end_cell])
          , key = "mdxtnq") %>%
          subset(mdxtnq != 0)    %>%   # remove data outside polygons (== zone_id = 0 OR NA)
          subset(!is.na(mdxtnq))

        # If out_data not empty (i.e., at least one pixel of current chunk becz_opts$longs to an
        # polygon), put out_data in all_data[[chubnk_n_all]], then compute cz_opts$summ_data
        if (dim(out_data)[1] > 0) {
          if (cz_opts$full_data) {
            all_data[[chunk_n_all]]  <- out_data
            chunk_n_all              <- chunk_n_all + 1
          }
          if (cz_opts$summ_data) {
            #   ____________________________________________________________________________
            #   verify if currently in out_data we have all the data corresponding   ####
            #   to any of the polygons. In that case, compute the summary statistics
            #   for those polygons, and remove their data from "out_data"
            temp_outdata   <- data.table::rbindlist(list(temp_outdata,out_data))
            check_n        <- temp_outdata %>%
              dplyr::group_by(mdxtnq) %>%
              dplyr::summarize(count = n())
            codes_in_chunk <- unique(out_data$mdxtnq)
            # polys_in_chunk <- subset(in_vect_zones_crop, mdxtnq %in% codes_in_chunk)
            complete_polys <- NA
            for (indcinc in seq_along(along.with = codes_in_chunk)) {
              cinc <- codes_in_chunk[indcinc]
              complete_polys[indcinc] <- (check_n[check_n$mdxtnq == cinc, 2] ==
                                            cells_per_poly[cells_per_poly$mdxtnq == cinc, 2])
            }
            #   ____________________________________________________________________________
            #   If we have all data for any polygon, compute the summary statistics for ####
            #   those, and remove them from "temp_outdata", then put temp_outdata in out_data
            if (any(complete_polys == TRUE)) {

              data_for_summary          <- subset(temp_outdata, mdxtnq %in% codes_in_chunk[complete_polys])
              stat_data[[chunk_n_summ]] <- fast_summ(data_for_summary, "mdxtnq", cz_opts$comp_quant, selbands[band], selband)
              temp_outdata              <- subset(temp_outdata, !(mdxtnq %in% codes_in_chunk[complete_polys]))
              chunk_n_summ              <- chunk_n_summ + 1
            }
          }
        }
        start_cell  <- end_cell + 1  # Increment start_cell to get the start of the next chunk
        # gc()
      } # end cycle on chunks

      #   ____________________________________________________________________________
      #   bind data from all chunks in `all_data`  and 'stat_data'                ####
      if (cz_opts$full_data) {

        all_data <- data.table::rbindlist(all_data) %>%
          data.table::setkey("mdxtnq")
      }

      if (cz_opts$summ_data) {
        stat_data <- data.table::rbindlist(stat_data) %>%
          data.table::setkey("mdxtnq")

      }

      #   ____________________________________________________________________________
      #   extract data for cz_opts$small polygons if requested and necessary              ####

      if (cz_opts$small & length(unique(stat_data$mdxtnq) != length(unique(in_vect_zones_crop$mdxtnq)))) {

        miss_feat <- setdiff(unique(in_vect_zones_crop$mdxtnq), unique(stat_data$mdxtnq))
        for (mfeat in miss_feat) {

          poly_miss         <- sp_polys[sp_polys@data$mdxtnq == mfeat,]
          data_feat         <- raster::extract(in_rast[[band]], poly_miss, small = TRUE,
                                            method = "simple", df = TRUE, cellnumbers = TRUE)
          cell              <- data_feat[,2]
          miss_feat_data    <- data.table::data.table(value = data_feat[,3],
                                       cell =  cell,
                                       mdxtnq = mfeat)
          if (cz_opts$full_data) {
            all_data        <- rbind(all_data, miss_feat_data)
            }
          # compute the summary statistics for the current "cz_opts$small feature"
          if (cz_opts$summ_data) {
            miss_feat_stats <- fast_summ(miss_feat_data, "mdxtnq", cz_opts$comp_quant, selbands[band], selband)
            stat_data       <- rbind(stat_data, miss_feat_stats)
          }
        }
      }

      # ____________________________________________________________________________
      # if cz_opts$full_data required, extract all pixels values, and add some additional   ####
      # column

      if (cz_opts$full_data) {
        all_data <- all_data[, list(band_n = selbands[band],
                                    date   = seldates[band],
                                    N_PIX  = seq(along.with = value),
                                    value  = value,
                                    cell   = cell
        ) , by = mdxtnq]
      }

      ##  ............................................................................
      ##  update progressbar                                                      ####
      if (cz_opts$verbose) {
        Sys.sleep(0.001)
        utils::setTxtProgressBar(pb, band)
      }

      #   ____________________________________________________________________________
      #   return data processed by the "worker" (a.k.a. the "band" results)       ####

      out <- list(alldata = all_data, stats = stat_data)
      return(out)

    } # End Foreach cycle on bands

  parallel::stopCluster(cl)

  if (cz_opts$verbose) message("comp_zonal --> Data extraction completed. Building outputs")

  # ___________________________________________________________________________________
  # End of data loading from in_rast. Now in all_data/stat_data we have all values  ####
  # needed to build the output
  #

  # --------------------------------------------------------------------------------
  # if cz_opts$keep_null selected and "outside features found, replace in_vect_zones_crop ####
  # with in_vect_zones, so that later joins "include" the missing features

  if (cz_opts$keep_null & !is.null(outside_feat)) {
    in_vect_zones_crop <- in_vect_zones
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the stats output list                       ####

  if (cz_opts$summ_data) {

    # "bind" the different bands
    stat_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 2)))
    # if cz_opts$addfeat, merge the extracted data with the shapefile features
    if (cz_opts$addfeat) {
      stat_data <- merge(stat_data, in_vect_zones_crop, by = "mdxtnq", all.y = TRUE)
    }

    # define the order of the output columns
    if (!cz_opts$comp_quant) {
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
    if (!cz_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    if (!cz_opts$addgeom) {
      keep_cols <- keep_cols[-length(keep_cols)]
      stat_data <- stat_data[, geometry := NULL]
      }
    if (!is.null(cz_opts$id_field)) {
      stat_data <- stat_data[, mdxtnq := NULL]
      keep_cols <- keep_cols[which(keep_cols != cz_opts$id_field)]
      keep_cols[1] <- eval(cz_opts$id_field)
    }
    # browser()
    # build the final output and convert to tibble
    stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
      as_tibble()

    if (is.null(cz_opts$id_field)) names(stat_data)[1] = "id_feat"

    # If `cz_opts$long`, reshape the stats output to a cz_opts$long table format

    if (cz_opts$long) {
      skip_cols <- ifelse(cz_opts$comp_quant,18, 6)
      n_adds    <- length(names_shp)
      idcols    <- c(seq(1,length(stat_data) - skip_cols - 1), length(stat_data))
      stat_data <- data.table::melt(stat_data, idcols)
      keep_cols <- c("id_feat",
                     "band_n", "date",
                     names_shp,
                     "variable", "value",
                     "geometry")
      if (!cz_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
      if (!cz_opts$addgeom) keep_cols <- keep_cols[-length(keep_cols)]
      if (!is.null(cz_opts$id_field)) {
        keep_cols    <- keep_cols[which(keep_cols != cz_opts$id_field)]
        keep_cols[1] <- eval(cz_opts$id_field)
      }
      stat_data <- data.table::setcolorder(stat_data, keep_cols) %>%
        tibble::as_tibble()
    }
    # If cz_opts$addgeom, convert to a sf object
    if (cz_opts$addgeom) {
      stat_data <- sf::st_as_sf(stat_data)
    }
  }

  #   ____________________________________________________________________________
  #   Reshuffle output to build the alldata list (includes adding the "point"
  #   coordinates and transforming to a `sf` object                           ####

  if (cz_opts$full_data) {
    # "bind" the different bands
    all_data <- data.table::rbindlist(do.call(c,lapply(results, "[", 1))) %>%
      data.table::setkey("mdxtnq")
    sf::st_geometry(in_vect_zones_crop) <- NULL

    # if cz_opts$addfeat, merge the extracted data with the shapefile features
    if (cz_opts$addfeat) {
      all_data <- merge(all_data, in_vect_zones_crop, by = "mdxtnq", all.y = TRUE)
    }
    # compute the coordinates of the extracted pixels and add them to all_data
    # then remove the "cell" column
    infast    <- velox::velox(in_rast[[1]])
    coords    <- infast$getCoordinates()[all_data$cell,]
    all_data  <- all_data[,c("x_coord", "y_coord") := list(coords[,1], coords[,2])]
    all_data  <- all_data[, cell := NULL]

    # define the order of the output columns
    keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX",
                   names_shp,
                   "value",
                   "x_coord", "y_coord")
    if (!cz_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    if (!is.null(cz_opts$id_field)) {
      keep_cols    <- keep_cols[which(keep_cols != cz_opts$id_field)]
      keep_cols[1] <- eval(cz_opts$id_field)
    }

    # build the final output and convert to tibble
    all_data  <- all_data[ , .SD, .SDcols = keep_cols] %>%
      tibble::as_tibble()

    # If cz_opts$addgeom, convert to a sf object
    if (cz_opts$addgeom) {
      all_data  <- sf::st_as_sf(all_data, coords = c("x_coord", "y_coord"), na.fail = FALSE) %>%
        sf::st_set_crs(sp::proj4string(in_rast))
    }
  }

  #   ____________________________________________________________________________
  #   Final cleanup                                                           ####

  # if dates were not passed, then change the name of column 3 to "band_name"
  if (!date_check) {
    if (cz_opts$summ_data) names(stat_data)[3] <- "band_name"
    if (cz_opts$full_data) names(all_data)[3]  <- "band_name"
  }

  if (!cz_opts$full_data) all_data  <- NULL
  if (!cz_opts$summ_data) stat_data <- NULL

  # create the final output list
  ts_out <- list(stats = stat_data, alldata = all_data)
  # gc()

  return(ts_out)
}
