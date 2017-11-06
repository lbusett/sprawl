#' @title convert DOYs to dates
#' @description Converts a doy (or array of doys) to date format. Year of origin
#'   has to be specified, to allow proper handling of leap years
#' @param doys 'numeric array' DOYs to be converted.
#' @param year 'numeric year to be used as basis.
#' @param verbose 'logical' if TRUE, print messages, default = TRUE
#' @return `Date` array resulting from the conversion
#' @details DETAILS
#' @examples
#'  # Convert doy 100 of 2015 to date
#'  doytodate(100, 2015)
#'
#' @rdname doytodate
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#' @export
doytodate <- function(doys, year, verbose = TRUE){

  if (!(is.numeric(doys) & is.numeric(year))) {
    stop("doytodate --> 'doys' and 'year' must be numeric ! Aborting !" )
  }

  if (!length(year) == 1) {
    stop("doytodate --> 'year' must be a single value ! Aborting !" )
  }
  # check for leap year (derived from `lubridate`)
  if (!((year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0)))) {
    range <- c(0,365)
  } else {
    range <- c(0,366)
  }

  # send message if outside range
  if (((min(doys, na.rm = TRUE) < min(range, na.rm = TRUE))) |
      (max(doys, na.rm = TRUE) > max(range, na.rm = TRUE))) {
    if (verbose) message(
      "doytodate --> `doy` is not within [0,365] (or [0,366] for leap years)! ",
      "Dates for doys outside this range will report a different year !" )
  }

  dates <- as.Date(doys - 1, origin = paste0(year, "-01-01"))
  return(dates)
}
