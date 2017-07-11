#' @title cz_polygons_std
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

cz_polygons_std <- function(in_vect_zones, in_rast, seldates, selbands, n_selbands, date_check, cz_opts) {

  #   ____________________________________________________________________________
  #   crop in_vect_zones to in_rast extent if necessary and identify "removed"   ####
  #   features + Find names of the attribute fields of the original shapefile

  if (cz_opts$verbose) message("comp_zonal --> Cropping the zones object on extent of the raster")
  #TODO  Do this only if extent of zone object is larger than that of the raster (in any direction)
  crop               <- cz_crop_object(in_vect_zones, in_rast, cz_opts$id_field, cz_opts$verbose)
  in_vect_zones_crop <- crop$in_vect_zones_crop
  outside_feat       <- crop$outside_feat
  names_shp          <- names(in_vect_zones_crop)[!names(in_vect_zones_crop) %in% c("mdxtnq", "geometry")]

  # Ientify the bboxes of polygons (required later to check if all data was loaded for a given
  # polygon when processing in "chunks")

  if (cz_opts$verbose) message("comp_zonal --> Computing bounding boxes of input polygons")

  bboxes <- in_vect_zones_crop %>%
    data.table::as.data.table()
  bboxes = bboxes[, list(min_y = sf::st_bbox(geometry)[2],
                   max_y = sf::st_bbox(geometry)[4]),
            by = "mdxtnq"]

  # find out the file on disk to which the raster stack refers to
  # in_rastfilename    <- in_rast@layers[[1]]@file@name

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

  if (cz_opts$verbose) {message("comp_zonal --> Rasterizing shape")}

  # define the extent of the zone object
  # zones_ext <- raster::extent(as.numeric(sf::st_bbox(in_vect_zones_crop))[c(1,3,2,4)])

  if (cz_opts$verbose) {message("comp_zonal --> Writing temporary shapefile")}
  temp_shapefile = tempfile(tmpdir = tempdir(), fileext = ".shp")
  writeshape(in_vect_zones_crop, temp_shapefile, overwrite = TRUE)

  # then convert it to raster
  if (cz_opts$verbose) {(message("comp_zonal --> Writing temporary rasterized shapefile"))}
  temp_rasterfile = tempfile(tmpdir = tempdir(), fileext = ".tiff")
  max_id <- max(in_vect_zones_crop$mdxtnq)
  ot <- dplyr::case_when(
    (max_id <= 255) == 1 ~ "Byte",
    (max_id >= 255 & max_id < 65535) == 1 ~ "Int16",
    (max_id >= 65536) == 1 ~ "Int32"
  )
  # cropped_rast_vrt <- tempfile(fileext = ".vrt")
browser()
  gdalpath <- getOption("gdalUtils_gdalPath")[[1]]$path
  rast_string <- paste("-tr", paste(cz_opts$rastres, collapse = " "),
                                    "-te", paste(raster::extent(in_rast)[c(1, 3, 2, 4)], collapse = " "),
                                    "-a", "mdxtnq",
                                    "-co" , "COMPRESS=DEFLATE",
                                    "-ot" , ot, sep = " ",
                       temp_shapefile,
                       temp_rasterfile
                       )
  system2(file.path(gdalpath, "gdal_rasterize"), args = rast_string)

  # gdalUtils::gdal_rasterize(temp_shapefile,
  #                           temp_rasterfile,
  #                           tr = cz_opts$rastres,
  #                           te = raster::extent(in_rast)[c(1, 3, 2, 4)],
  #                           a  = "mdxtnq",
  #                           co = c("COMPRESS=DEFLATE"),
  #                           ot = ot)

  # cropped_zones_vrt <- tempfile(fileext = ".vrt")
  # gdalUtils::gdalbuildvrt(temp_rasterfile,
  #                         cropped_zones_vrt,
  #                         te = sf::st_bbox(in_vect_zones_crop)
  # )
  rast_zoneobject <- raster::raster(temp_rasterfile)

  # rast_zoneobject <- velox::velox(temp_rasterfile)


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
  nrows             <- raster::nrow(rast_zoneobject)
  ncols             <- raster::ncol(rast_zoneobject)
  n_cells           <- nrows * ncols
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

  # results <- foreach::foreach(band = 1:n_selbands,
  #                             .packages = c("gdalUtils", "raster",
  #                                           "dplyr", "tibble",
  #                                           "data.table", "sf", "velox"),
  #                             .verbose = TRUE,
  #                             .options.snow = opts) %dopar%
      # {
  for (band in 1:1) {
browser()
    if (cz_opts$verbose) {
      message("comp_zonal--> Extracting data from ", ifelse(date_check, " dates", "bands"), " - Please wait !")
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

    #   ____________________________________________________________________________
    #   Get current band as a `velox` object and crop it on polygons extent     ####

    if (supersample) {
      #   in_band    <- velox::velox(in_rast[[selbands[band]]])
      #   in_band$crop(sp_polys)
      # } else {
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

      if (cz_opts$verbose) message("Working on chunk: ", chunk, " of: ", n_chunks, " of band: ", selband)

      # Identify row numbers of the current "chunk" ----
      startrow   <- ifelse(chunk == 1, 1, 1 + (chunk - 1) * ceiling(nrows / n_chunks))
      chunkrows  <- ifelse(chunk != n_chunks,
                           ceiling(nrows / n_chunks),
                           (1 + nrows - startrow))
      endrow      <- startrow + chunkrows - 1
      ncells      <- ncols * chunkrows
      end_cell    <- start_cell + ncells - 1

      print(paste(startrow, endrow, chunkrows))
      #   ________________________________________________________________________
      #   retrieve data of current "chunk" for the pixels included in the polygons ####
      #   and put it in all_data[[chunk_n]] (if not null)

      out_data  <- data.table::data.table(
          value   =  as.numeric(raster::getValues(in_band, startrow, chunkrows)),
          cell    =  seq(start_cell,end_cell),
          mdxtnq  =  as.numeric(raster::getValues(rast_zoneobject, startrow, chunkrows)),
        key = "mdxtnq")
      out_data <- out_data[mdxtnq != 0]

      ext_chunk <- data.frame(x_min = raster::extent(in_band)[1],
                              x_max = raster::extent(in_band)[2],
                              y_min = raster::yFromRow(in_band, endrow),
                              y_max = raster::yFromRow(in_band, 1)
                              )

      # If out_data not empty (i.e., at least one pixel of current chunk becz_opts$longs to an
      # polygon), put out_data in all_data[[chubnk_n_all]], then compute cz_opts$summ_data
      if (dim(out_data)[1] > 0) {
        if (cz_opts$full_data) {

          if (cz_opts$addgeom) {
            # Here we create a temporary "velox" raster, allowing to quickly compute coordinates
            # and save coordinates for the chunk in the "coords" list

            temp_velox <- velox::velox(matrix(nrow = chunkrows, ncol = ncols),
                                       ext = as.numeric(ext_chunk),
                                       res = cz_opts$rastres,
                                       crs = sp::proj4string(in_band))

            # Note: here out_data$cell - start_cell makes so that the first cell of the chunk
            # ends up in position 1 in the temporary velox (otherwise, out_data$cell is > than
            # the number of cells in tempvelox, after the first chunk)

            coords   <- temp_velox$getCoordinates()[(out_data$cell - start_cell),]

            # add coordinates to the data table and remove the "cell" column
            out_data <- out_data[,c("cell", "x_coord", "y_coord") :=
                                   list(NULL, coords[,1], coords[,2])]

          } else {
            out_data <- out_data[, cell := NULL]
          }

          all_data[[chunk_n_all]] <- out_data
          chunk_n_all             <- chunk_n_all + 1

        }
        if (cz_opts$summ_data) {
          #   ____________________________________________________________________________
          #   verify if currently in out_data we have all the data corresponding   ####
          #   to any of the polygons. In that case, compute the summary statistics
          #   for those polygons, and remove their data from "out_data" if full_data = FALSE)

          # get the extent of the area analysed so far on the basis of the coordinates of the
          # last row "loaded"

          tot_ext_y      <- data.frame(y_min = (raster::yFromRow(in_band, endrow)   - cz_opts$rastres[1]/2),
                                  y_max = (raster::yFromRow(in_band, 1) + cz_opts$rastres[1]/2))
          temp_outdata   <- data.table::rbindlist(list(temp_outdata,out_data))
          complete_polys <- bboxes %>%
            dplyr::filter(min_y >= tot_ext_y$y_min)

          # for (indcinc in seq_along(along.with = codes_in_chunk)) {
          #   cinc <- codes_in_chunk[indcinc]
          #   complete_polys[indcinc] <- (check_n[check_n$mdxtnq == cinc, 2] >=
          #                                 cells_per_poly[, cells_per_poly$mdxtnq == cinc])
          # }
          #   ____________________________________________________________________________
          #   If we have all data for any polygon, compute the summary statistics for ####
          #   those, and remove them from "temp_outdata", then put temp_outdata in out_data
          if (length(complete_polys$mdxtnq) != 0) {

            data_for_summary          <- subset(temp_outdata, mdxtnq %in% unique(complete_polys$mdxtnq)) %>%
                                            data.table::setkey("mdxtnq")

            stat_data[[chunk_n_summ]] <- fast_summ(data_for_summary,
                                                   "mdxtnq",
                                                   cz_opts$comp_quant,
                                                   cz_opts$FUN,
                                                   selbands[band],
                                                   selband)
            temp_outdata              <- subset(temp_outdata, !(mdxtnq %in% unique(complete_polys$mdxtnq)))
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

        poly_miss       <- in_vect_zones_crop %>%
          dplyr::filter(mdxtnq == mfeat) %>%
          sf::st_as_sf() %>%
          as("Spatial")
        data_feat       <- raster::extract(in_rast[[band]], poly_miss, small = TRUE,
                                           method = "simple", df = TRUE, cellnumbers = TRUE)
        cell            <- data_feat[,2]
        miss_feat_data  <- data.table::data.table(value = data_feat[,3],
                                                  cell =  cell,
                                                  mdxtnq = mfeat)
        if (cz_opts$addgeom) {
          coords <- raster::xyFromCell(in_band, cell)
          miss_feat_data <- miss_feat_data[,c("cell", "x_coord", "y_coord") :=
                                            list(NULL, coords[,1], coords[,2])]
        } else {
          miss_feat_data <- miss_feat_data[,cell := NULL]
        }
        if (cz_opts$full_data) {
          all_data        <- rbind(all_data, miss_feat_data)
        }
        # compute the summary statistics for the current "cz_opts$small feature"
        if (cz_opts$summ_data) {
          miss_feat_stats <- fast_summ(miss_feat_data,
                                       "mdxtnq",
                                       cz_opts$comp_quant,
                                       cz_opts$FUN,
                                       selbands[band],
                                       selband)
          stat_data       <- rbind(stat_data, miss_feat_stats)
        }
      }
    }

    # ____________________________________________________________________________
    # if cz_opts$full_data required, extract all pixels values, and add some additional   ####
    # column

    if (cz_opts$full_data) {

      # this computes number of pixels per polygon and gives a sequential number
      # to each pixel in the polygon (https://goo.gl/c83Pfd)

      all_data <- all_data[, c("band_n", "date", "N_PIX", "N") :=
                             list(band, seldates[band], .N, seq_len(.N)), by = mdxtnq]
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

    if (!is.null(cz_opts$FUN)) {
      keep_cols <- c("mdxtnq", "band_n", "date",
                     names_shp,
                     "N_PIX", "myfun",
                     "geometry")
   } else {

      if (!cz_opts$comp_quant) {
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
    # infast    <- velox::velox(in_rast[[1]])
    # coords    <- infast$getCoordinates()[all_data$cell,]

    # coords <- data.table::rbindlist(do.call(c,lapply(results, "[", 3))) %>%
    #   data.table::setkey("mdxtnq")
    # all_data  <- all_data[,c("x_coord", "y_coord") := list(coords[,1], coords[,2])]
    # all_data  <- all_data[, cell := NULL]

    # define the order of the output columns
    keep_cols <- c("mdxtnq", "band_n", "date", "N_PIX", "N",
                   names_shp,
                   "value",
                   "x_coord", "y_coord")
    if (!cz_opts$addfeat) keep_cols <- keep_cols[which(!keep_cols %in% names_shp)]
    if (!cz_opts$addgeom) keep_cols <- keep_cols[which(!keep_cols %in% c("x_coord", "y_coord"))]
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

  # file.remove(temp_rasterfile)
  # file.remove(temp_shapefile)
  # gc()

  return(ts_out)
}
