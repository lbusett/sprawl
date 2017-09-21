#' @title er_getbands
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
#' er_getbands(in_rast, selbands = c(1, 2))
#' er_getbands(in_rast, selbands = c("2013-01-01", "2015-12-31"))
#' }
er_getbands <- function(in_rast,
                        selbands = NULL,
                        verbose  = FALSE) {

  banderr_msg <- glue::glue(
    "extract_rast --> `selbands` must be either a 2-element numeric array\n",
    "containing starting and ending band numbers for the extraction OR a\n ",
    "2-element array containing starting and ending Dates for the extraction."
  )
  # Check if in_rast is a raster object or file -----
  if (get_rastype(in_rast) == "rastfile") {
    in_rast <- raster::stack(in_rast)
  }
  if (get_rastype(in_rast) != "rastobject") {
    stop("`in_rast` is not a valid *raster object or raster file! Aborting !")
  }
  nbands <- raster::nlayers(in_rast)

  # Check if dates attributes are present in in_rast -----

  if (class(raster::getZ(in_rast)) == "Date") {
    dates <- raster::getZ(in_rast)
    date_chk <- 1
  } else {
    date_chk <- 0
  }


  if (is.null(selbands)) {
    selbands_out <- c(1,nbands)
    seldates     <- NA
  }

  # Check if selbands is a 2-element integer array -----
  if (is.numeric(selbands)) {
    if (length(selbands[!is.na(selbands)]) != 2) stop(banderr_msg)
    selbands_out <- selbands
    seldates     <- NA
  } else {
    # Check if selbands is a 2-element character array coercible to date -----
    if (is.character(selbands)) {
      seldates <- try(as.Date(selbands), silent = TRUE)
      if (class(seldates) == "try-error") stop(banderr_msg)
      if (length(seldates[!is.na(seldates)]) != 2) stop(banderr_msg)
      # dates <- seldates
    } else {
      # Check if selbands is a 2-element Date array -----
      if (lubridate::is.timepoint(selbands)) {
        seldates <- try(as.Date(selbands), silent = TRUE)
        if (class(seldates) == "try-error") stop(banderr_msg)
        if (length(seldates[!is.na(seldates)]) != 2) stop(banderr_msg)
        if (seldates[2] < seldates[1]) {
          stop("extract_rast --> Ending band/date is lower than Starting band/date. ",
               "Aborting !")
        }
        # dates <- seldates
      }
    }

  }



  # Input raster contains valid date information AND dates are passed as
  # argument

  if (date_chk & lubridate::is.timepoint(seldates)) {
    selbands_out <- c(NA,NA)

    dates_post_start <- which(dates >= seldates[1])
    dates_pre_end    <- which(dates > seldates[2])
    if (length(dates_post_start) != 0) {
      selbands_out[1] <- min(dates_post_start)
    } else {
    stop("extract_rast --> Start band/date (selbands[1]) greater than number ",
         "of available bands/later than first available date. Aborting!")
    }
    if (length(dates_pre_end) != 0) {
      selbands_out[2] <- min(dates_pre_end) - 1
    } else {
      selbands_out[2] <- nbands
      # stop("extract_rast --> End band/date (selbands[2]) lower than number ",
      #      "of available bands/later than first available date. Aborting!")
    }
  } else {
    # selbands provided as dates, but no dates in input raster --> aborting
    if (lubridate::is.timepoint(seldates) & !date_chk) {
      stop("extract_rast --> Input raster doesn't contain valid dates in its",
           "'Z' attribute.\nPlease specify the layers to be processed using",
           "a numeric array (e.g., selbands = c(1,5)). Aborting!")
    }
  }

  # Check for consistency in selbands -----
  if (selbands_out[1] > nbands) {
    if (!lubridate::is.timepoint((selbands_out[1]))) {
      stop("extract_rast --> Start band (selbands[1]) greater than number ",
           "of available layers ! Aborting !")
    } else {
      stop("extract_rast --> Start date (selbands[1]) later than last date ",
           "available in `in_rast` ! Aborting !")
    }
  }

  if (selbands_out[2] < selbands_out[1]) {
    if (!lubridate::is.timepoint((selbands_out[2]))) {
      stop("extract_rast --> End band (selbands[2]) smaller than start band ",
           "(selbands[1]) ! Aborting !")
    } else {
      stop("extract_rast --> End date (selbands[2]) later than start date ",
           "(selbands[1]) ! Aborting !")
    }
  }

  if (selbands_out[2] > nbands) {
    if (!lubridate::is.timepoint((selbands_out[2]))) {
      warning("extract_rast --> End band (selbands[2]) greater than number of ",
              "available layers ! Resetting End band to nlayers")
    } else {
      warning("extract_rast --> End date (selbands[2]) later than last date ",
              "available in `in_rast` ! Resetting End band to nlayers")
    }
    selbands_out[2] <- nbands
  }
  selbands_out <- seq(selbands_out[1],selbands_out[2],1)
  attr(selbands_out, "date_check") <- date_chk
  return(selbands_out)
}

