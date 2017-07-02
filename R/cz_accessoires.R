#' @title cz_getbands
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param selbands PARAM_DESCRIPTION, Default: NA
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @export
#' @importFrom lubridate is.timepoint
#' @importFrom raster stack nlayers getZ
#' @examples
#' \dontrun{
#' cz_getbands(in_rast, selbands = c(1, 2))
#' cz_getbands(in_rast, selbands = c("2013-01-01", "2015-12-31"))
#' }
cz_getbands <- function(in_rast,
                        selbands = NULL,
                        verbose  = FALSE) {

  banderr_msg <-"comp_zonal --> `selbands` must be either a 2-element numeric array containing starting and ending
         band numbers for the extraction OR a 2-element array containing starting and ending Dates for the extraction"
  # Check if in_rast is a raster object or file -----
  if (check_spatype(in_rast) == "rastfile") {
    in_rast <- raster::stack(in_rast)
  }
  if (check_spatype(in_rast) != "rastobject") {
    stop("`in_rast` is not a valid *raster object or raster file ! Aborting !")
  }
  nbands <- raster::nlayers(in_rast)

  # Check if selbands is a 2-element integer or dates array -----
  if (is.null(selbands)) {
    selbands_out = c(1,nbands)
  } else {
    if (is.character(selbands)) {
      seldates <- try(as.Date(selbands), silent = TRUE)
      if (class(seldates) == "try-error") stop(banderr_msg)
      if (length(seldates[!is.na(seldates)]) != 2) stop(banderr_msg)
      selbands_out <- seldates
    } else {
      if (!(is.numeric(selbands) & length(selbands[!is.na(selbands)]) == 2))  stop(banderr_msg)
      selbands_out <- selbands
    }
  }
  # Check if dates attributes are present in in_rast -----
  date_chk <- 0
  if (!class(raster::getZ(in_rast)) == "Date") {
    if(verbose) message("comp_zonal --> Input doesn't contain valid dates in its 'Z' attribute. Band numbers will be used instead
    on the outputs")
    dates <-  NA
  } else {
    dates <- raster::getZ(in_rast)
    date_chk <- 1
    if ((max(is.na(dates))) != 0) {
      warning("comp_zonal --> Dates of input time series raster contain some NA values - They will be ignored !")
      dates <- NA
      date_chk <- 0
    }
  }

  # Input raster contains valid date information AND dates are passed as argument

  if (date_chk & lubridate::is.timepoint((selbands_out))) {
    selbands_out    <- c(NA,NA)
    selbands_out[1] <- min(which(dates >= selbands[1]))
    selbands_out[2] <- min(which(dates > selbands[2])) - 1
  }


  # Check for consistency in selbands -----
  if (selbands_out[1] > nbands) {
    if (!lubridate::is.timepoint((selbands_out[1]))) {
      stop("comp_zonal --> Start band (selbands[1]) greater than number of available layers ! Aborting !")
    } else {
      stop("comp_zonal --> Start date (selbands[1]) later than last date available in `in_rast` ! Aborting !")
    }
  }

  if (selbands_out[2] < selbands_out[1]) {
    if (!lubridate::is.timepoint((selbands_out[2]))) {
      stop("comp_zonal --> End band (selbands[2]) smaller than start band (selbands[1]) ! Aborting !")
    } else {
      stop("comp_zonal --> End date (selbands[2]) later than start date (selbands[1]) ! Aborting !")
    }
  }

  if (selbands_out[2] > nbands) {
    if (!lubridate::is.timepoint((selbands_out[2]))) {
      warning("comp_zonal --> End band (selbands[2]) greater than number of available layers ! Resetting End band to nlayers")
    } else {
      warning("comp_zonal --> End date (selbands[2]) later than last date available in `in_rast` ! Resetting End band to nlayers")
    }
    selbands_out[2] <- nbands
  }
  selbands_out <- seq(selbands_out[1],selbands_out[2],1)
  attr(selbands_out, "date_check") <- date_chk
  return(selbands_out)
}

#' @title cz_crop_object
#' @description Accessory function for `comp_zonal`. Crops the zone_object on the extent of the raster
#' to improve speed. Then check if some features in the original zone_object were dropped in
#' zone_raster_croppped
#' @param zone_object PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param id_field PARAM_DESCRIPTION
#' @return `list` with the following fields:
#' - zone_object_crop = cropped `sf` object
#' - outside_feat = data frame containg info on the "removed" features (sequential id and corresponding
#' value ofthe "id" field (if provided)

#' @importFrom raster extent
#' @importFrom sf st_intersection st_set_crs st_as_sf st_crs st_bbox
#' @importFrom tibble as_data_frame
cz_crop_object <- function(zone_object,
                           in_rast,
                           id_field) {


  #   ____________________________________________________________________________
  #   Crop input `sf` object to the extent of the input raster                ####

  zone_object_crop <- sf::st_intersection(zone_object,
                                          sf::st_set_crs(sf::st_as_sf(as(raster::extent(in_rast[[1]]), "SpatialPolygons")),
                                                         sf::st_crs(zone_object)))
  #   ____________________________________________________________________________
  #   Check if the cropped file has different extension (i.e., it was croppped) ####
  #   If so, check if any features were dropped because outside the raster extent
  #   and save the "ids" of those features in `outside_feat`

  if (!isTRUE(all.equal(sf::st_bbox(zone_object_crop),
                        (sf::st_bbox(zone_object)), scale = 100))) {
    warning("Some features of the spatial object are outside or partially outside the extent of the input RasterStack !
  Outputs for features only partially inside will be retrieved using only the available pixels !
  Outputs for features outside rasterstack extent will be set to NA."
    )
    if (!setequal(zone_object$mdxtnq, zone_object_crop$mdxtnq)) {

      outside_ids   <- setdiff(zone_object$mdxtnq, zone_object_crop$mdxtnq)
      outside_names <- ifelse(!is.null(id_field),
                              as.character(tibble::as_data_frame(zone_object[outside_ids,eval(id_field)])[,1]),
                              rep(NA, length(outside_ids)))
      outside_feat  <- data.frame(outside_ids   = zone_object$mdxtnq[outside_ids],
                                  outside_names = outside_names)

    } else {
      outside_feat  <- NULL
    }
  } else {
    outside_feat  <- NULL
  }
  return(list(zone_object_crop = zone_object_crop, outside_feat = outside_feat))
}
