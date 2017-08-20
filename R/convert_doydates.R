#' @title convert DOYs to dates
#' @description Converts a doy (or array of doys) to date format. Year of origin
#'   has to be specified, to allow proper handling of leap years
#' @param doys 'numeric array' DOYs to be converted.
#' @param year 'numeric year to be used as basis.
#' @param verbose 'logical' if TRUE, print messages, default = TRUE
#' @return `Date` array resulting from the conversion
#' @details DETAILS
#' @examples
#' \dontrun{
#'  doytodate(100, 2015)
#' }
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
  if (((min(doys) < min(range))) | (max(doys) > max(range))) {
    if (verbose) message(
      "doytodate --> `doy` is not within [0,365] (or [0,366] for leap years)! ",
      "Dates for doys outside this range will report a different year !" )
  }

  dates <- as.Date(doys - 1, origin = paste0(year, "-01-01"))
  return(dates)
}

#' @title Convert dates to DOYs
#' @description Converts an array of `Date` (or coercible to `Date`) values to
#'   DOY (Day Of the Year) values. Simple wrapper around strftime(date, format = \"\%j\").
#' @param dates array of dates to be converted. Class must be `Date` or character
#'   parsable to `Date` using [`as.Date`] (format "yyyy-mm-dd").
#' @examples
#' \dontrun{
#'  datetodoy(as.Date("2000-04-01"))
#' }
#' @export
#' @rdname datetodoy
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>

datetodoy <- function(dates = dates){
  dates <- as.Date(dates)
  if (is.na(max(dates))) {
    stop("datetodoy --> some input 'dates' can not be parsed to `Date` ",
         "objects ! Aborting !" )
  }
  doys <- as.numeric(strftime(dates, format = '%j'))
  return(doys)
}
