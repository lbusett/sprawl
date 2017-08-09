#' fastzonal
#'
#' @description aaaa
#'
#' @param in_rast a
#' @param zone_object f
#' @param end_date g
#' @param id_field g
#' @param FUN h
#' @param out_format g
#' @param small h
#' @param small_method h
#' @param na.rm g
#' @param verbose g
#' @param start_band t
#' @param end_band re
#' @param maxchunk f
#' @param mask_object g
#'
#' @importFrom xts as.xts
#' @importFrom rgdal writeOGR readOGR
#' @importFrom sp proj4string spTransform CRS
#' @importFrom tools file_path_sans_ext
#' @importFrom raster getValues crop extent getZ extract rasterize res nlayers
#' @importFrom tools file_path_sans_ext
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom lubridate ymd
#' @return Time series
#' @export
#'
#' @examples
fastzonalssss = function(in_rast,
                     zone_object,
                     mask_object = NULL,
                     start_date  = NULL,
                     end_date    = NULL,
                     start_band  = NULL,
                     end_band    = NULL,
                     id_field    = NULL,
                     FUN         = "mean",
                     out_format  = "xts",
                     small       = TRUE,
                     small_method = "centroids",
                     na.rm       = TRUE,
                     verbose     = FALSE,
                     maxchunk    = 50E6,
                     add_null    = FALSE)
{
  # Check input types and requesed dates/bands -----
  # Check input types and requesed dates/bands -----

  ras_type   <-  check_spatype(in_rast)
  zone_type  <-  check_spatype(zone_object)
  if (!ras_type %in% "rastobject") {
    stop("Input in_rast is not a RasterStack or RasterBrick object")
  }
  if (zone_type == "none") {
    stop("Input zone_object is not a valid spatial file or object !")
  }

  if (!class(getZ(in_rast)) == "Date") {
    message("Input doesn't contain valid dates in its 'Z' attribute\nBand numbers will be used instead on the outputs")
    ts_check = FALSE
  } else {
    dates <- getZ(in_rast)
    ts_check = TRUE
  }

  if (is.null(start_date) & is.null(start_band)) {
    if (ts_check) {start_band = ymd(dates[1])} else {start_band = as.integer(1)}
    if (verbose) {message("Starting date/Starting band not provided - Using the first layer in the stack")}
  }

  if (is.null(end_date) & is.null(end_band)) {
    if (ts_check) {end_band = ymd(dates[nlayers(in_rast)])} else {end_band = as.integer(nlayers(in_rast))}
    if (verbose) {message("Starting date/Starting band not provided - Using the last layer in the stack")}
  }
  if (!class(start_band) %in% c("Date", "POSIXct", "POSIXlt")) {
    start_date = try(as.Date(start_date), silent = TRUE)
    if (class(start_date) == "try-error") {
      warning("start_date is not a Date object or string coercible to date - it will be ignored")
      start_date <- as.integer(1)
    }
  }
  if (!class(end_band) %in% c("Date", "POSIXct", "POSIXlt")) {
    end_date = try(as.Date(end_date), silent = TRUE)
    if (class(end_date) == "try-error") {
      warning("end_date is not a Date object or string coercible to date - it will be ignored")
      end_date <- nlayers(in_rast)
    }
  }

  # if (!class(start_band) == "integer") { stop("start_band is not numeric") }
  # if (!class(end_band)   == "integer") { stop("end_band is not numeric")}

  if (start_band > end_band) {
    stop("start_date larger than end_date")
  }

  if (!small_method %in% c("centroids", "full")) {
    warning("Unknown 'small_method' value - resetting to 'centroids'")
  }
  if (!out_format %in% c("xts", "dframe")) {
    if (verbose)
      message("Unknown 'out_format' value - resetting to 'xts'")
    out_format = "xts"
  }



  if (!ts_check) {
    dates <- seq(1, nlayers(in_rast), 1)
  }

  # if not dates passed, or in_rast doesn't have dates, requested dates
  # replaced by band numbers
  if (class(start_date) == "integer" & class(end_date) == "integer") {
    sel_dates <- seq(start_band, end_band, 1)
  } else {
    sel_dates <- which(dates >= start_band & dates <= end_band)
  }
  # Start cycling on dates/bands -----
  if (length(sel_dates) > 0) {


    if (check_spatype(zone_object) %in% c("vectfile", "spobject")) {

      if (check_spatype(zone_object) == "vectfile") {zone_object <- readshape(zone_object)}

      if (length(id_field) != 0) {
        if (!id_field %in% names(zone_object)) {
          warning(
            "Invalid 'id_field' value - names of output columns will be the record number of the shapefile feature"
          )
          id_field <- NULL
        }
      }
      if (proj4string(zone_object) != proj4string(in_rast)) {
        zone_object <- spTransform(zone_object, CRS(proj4string(in_rast[[1]])))
      }

      zone_object@data$mdxtnq = seq(1:length(zone_object@data[, 1]))
      zone_cropped = crop(zone_object, extent(in_rast[[1]]))

      if (!isTRUE(all.equal(extent(zone_cropped), (extent(zone_object)), scale = 100))) {
        warning(
          "Some features of the spatial object are outside or partially outside\n the extent of the input RasterStack !
Output for features outside rasterstack extent\n will be set to NODATA. Outputs for features only partially inside\n
will be retrieved\n using only the available pixels !"
        )
        if (!setequal(zone_object$mdxtnq, zone_cropped$mdxtnq)) {
          outside_feat = setdiff(zone_object$mdxtnq, zone_cropped$mdxtnq)
        }
      }
      # Extraction on points  or lines -----

      if (class(zone_cropped) %in% c("SpatialPointsDataFrame","SpatialPoints","SpatialLines",
                                     "SpatialLinesDataFrame")) {
        if (verbose) { message("On point and lines shapefiles, the standard `extract` function is used.
              This could be slow !")}

        ts <- matrix(nrow = length(sel_dates), ncol = length(zone_cropped[, 1]))

        for (f in 1:length(sel_dates)) {
          if (verbose == TRUE) {
            message(paste0("Extracting data from ", ifelse(ts_check, "date: ", "band: "),
                           dates[sel_dates[f]]))
          }
          ts[f,] <- raster::extract(in_rast[[sel_dates[f]]], zone_cropped, fun = FUN)
        }
        ts <- as.data.frame(ts)
        if (length(id_field) == 1) {

          all_feats <- as.character(zone_cropped@data[, eval(id_field)])
          names(ts) <- c(all_feats)
        }
        else {
          names(ts) <- 1:length(zone_cropped[, 1])
          all_feats <- as.character(names(ts))
        }
        if (out_format == "dframe") {
          ts <- cbind(date = dates[sel_dates], ts)
        }
      } else {
        if (check_spatype(zone_object) != "rastfile") {


          # Extraction on polygons: raterize shape ----
          # browser()
          if (verbose) { message("Rasterizing shape")}
          if (verbose) { message("Writing temporary shapefile")}

          tempshape = tempfile(tmpdir = tempdir(), fileext = ".shp")
          writeOGR(zone_cropped, dsn = dirname(tempshape),layer = basename(file_path_sans_ext(tempshape)),
                   driver = "ESRI Shapefile", overwrite_layer = TRUE,verbose = FALSE)

          if (verbose) {(message("Writing temporary rasterized shapefile"))}
          tempraster = tempfile(tmpdir = tempdir(), fileext = ".tiff")

          if (max(zone_cropped@data$mdxtnq) <= 255) {
            ot = "Byte"
          } else {
            ot <- ifelse(max(zone_cropped@data$mdxtnq) <= 65536, "Int16", "Int32")
          }
          gdal_rasterize(tempshape, tempraster, tr = raster::res(in_rast), te = extent(in_rast)[c(1, 3, 2, 4)], a = "mdxtnq", ot = ot)
          rastzone_object <- raster(tempraster)
        }

        else {
          rastzone_object = raster(zone_object)
          rastzone_object = crop(rastzone_object, extent(in_rast[[1]]))
        }
        # Extraction on raster: crop raster on shape ----



        # Setup chunks ----
        n_cells   <- nrow(rastzone_object) * ncol(rastzone_object)
        ncols     <- ncol(rastzone_object)
        n_chunks  <- floor(n_cells / maxchunk)
        full_data <- list()

        # Start data extraction ----
        for (f in 1:length(sel_dates)) {

          if (verbose == TRUE) {
            message(paste0("Extracting data from date: ",
                           dates[sel_dates[f]]))
          }

          if (n_chunks > 1) {
            for (chunk in seq_len(n_chunks)) {

              # Import data chunk ----
              startrow <- ifelse(chunk == 1, 1, (chunk - 1) * ceiling(nrow(rastzone_object) / n_chunks))
              nrows    <- ifelse(chunk != n_chunks, ceiling(nrow(rastzone_object) / n_chunks),
                                 nrow(rastzone_object))
              message(chunk, " ", startrow, " ",  startrow + (nrows - 1))
              # put current chunk in "full_data
              full_data[[chunk]] <- data.table(
                value = getValues(in_rast[[sel_dates[f]]], startrow, ifelse(chunk == 1, nrows - 1, nrows)),
                zones = getValues(rastzone_object, startrow, ifelse(chunk == 1, nrows - 1, nrows)),
                key = 'zones') %>%
                subset( zones != 0) # remove data outside polygons (== zones = 0 )
              gc()
            }
            gc()
            # Add to the full data ----
            # browser()
            full_data <- rbindlist(full_data)

          } else {
            full_data <- data.table(
              value = getValues(in_rast[[sel_dates[f]]]), zones = getValues(rastzone_object)) %>%
              subset( zones != 0) # remove data outside polygons (== zones = 0 )
          }

          setkey(full_data, "zones")

          if (f == 1) {
            zones <- unique(full_data$zones)
            ts <- matrix(nrow = length(sel_dates), ncol = length(zones))
            if (length(id_field) == 1) {

              all_feats <- as.character(zone_cropped@data[, eval(id_field)])
              names(ts) <- c(all_feats)
            }
            else {
              names(ts) <- 1:length(zone_cropped[, 1])
              all_feats <- as.character(names(ts))
            }
          }

          # Apply the aggregation function if needed, otherwise just extract all pixrels
          if (FUN != 'null') {

            ts[f,] <- full_data[, lapply(.SD, match.fun(FUN),
                                         na.rm = na.rm), by = zones]$value

          } else {
            browser()
          }
        }
      }
      # Put the correct names on columns -----
      # ts <- as.data.frame(ts)

      if (zone_type == "spobject") {
        if (length(id_field) == 1) {
          feat_names <- as.character(zone_cropped@data[,
                                                      eval(id_field)])[sort(unique(zones))]
          names(ts) <- feat_names
        } else {
          feat_names <- as.character(zone_cropped@data[, "mdxtnq"])[sort(unique(zones))]
          names(ts) <- feat_names
        }
      } else {
        feat_names <- as.character(sort(unique(zones)))
        names(ts) <- feat_names
      }

      # On input shapefile: add "missing features"  using small poly extraction if requested ----

      # if (zone_type %in% c("spobject", "vectfile")) {
      #   if (small & ncols != length(all_feats)) {
      #     if (length(id_field) == 1) {
      #       miss_feat   <- setdiff(as.character(zone_cropped@data[, "mdxtnq"]), names(ts))
      #       pos_missing <- which(as.character(zone_cropped@data[, "mdxtnq"]) %in% miss_feat)
      #     } else {
      #       pos_missing <- miss_feat <- which(as.character(zone_cropped@data[, "mdxtnq"])
      #                                         %in% names(ts))
      #     }
      #     shpsub <- zone_cropped[pos_missing,]
      #     ts_mis <- matrix(nrow = length(sel_dates), ncol = length(pos_missing))
      #     for (f in 1:length(sel_dates)) {
      #       if (verbose == TRUE) {
      #         print(paste0("Extracting data from date: ",
      #                      dates[sel_dates[f]]))
      #       }
      #       if (small_method == "centroids") {
      #         ts_mis[f,] <- extract(in_rast[[sel_dates[f]]],
      #                               sp::coordinates(shpsub), fun = mean)
      #       } else {
      #         ts_mis[f,] <- extract(in_rast[[sel_dates[f]]],
      #                               shpsub, fun = mean)
      #       }
      #     }
      #     colnames(ts_mis) <- miss_feat
      #     ts <- cbind(ts, ts_mis)
      #   }
      # }
      #
      if (zone_type == "spobject") {
        file.remove(tempraster)
        file.remove(tempshape)
        # If there are features outside the raster extent, set the correspondig output to NA -----
      #   if (exists("outside_feat")) {
      #     if (length(id_field) == 1) {
      #       feat_names_outside = as.character(zone_object@data[, eval(id_field)])[outside_feat]
      #     }
      #     else {
      #       feat_names_outside = as.character(zone_object@data[, "mdxtnq"])[outside_feat]
      #     }
      #
      #     ts_outside = matrix(nrow = length(sel_dates),
      #                         ncol = length(feat_names_outside))
      #     ts_outside = data.frame(ts_outside)
      #
      #     names(ts_outside) = feat_names_outside
      #     ts = cbind(ts, ts_outside)
      #     names(ts) = c(feat_names,feat_names_outside)
      #
      #     # if (length(id_field) == 1) {
      #     #   sortindex = match(zone_object@data[, eval(id_field)], names(ts))
      #     # } else {
      #     #   sortindex = match(zone_object@data[, "mdxtnq"], names(ts))
      #     # }
      #     # ts = ts[, c(1, sortindex)]
      #   # }

      }
    }
    # Convert output to xts if necessary ------
    if (out_format == "xts") {
      ts <- as.xts(ts, order.by = dates[sel_dates])
    }


    # prepare data.frame output if needed ----
    if (out_format == "dframe") {
      ts <- cbind(dates[sel_dates], ts)
      names(ts)[1] = "date"
    }

    gc()
    return(ts)
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
  gc()
  return(ts)
}
