#' @title Convert dates to DOYs
#' @description Converts an array of `Date` (or coercible to `Date`) values to
#'   DOY (Day Of the Year) values. Simple wrapper around strftime(date, format = \"\%j\").
#' @param dates array of dates to be converted. Class must be `Date` or character
#'   parsable to `Date` using [`as.Date`] (format "yyyy-mm-dd").
#' @param abort `logical` If TRUE, the function aborts if even just one of the
#'   dates fails converting (i.e., because one date is NA). If FALSE, a warning
#'   is issued instead, Default: FALSE
#' @examples
#'  # Convert a date to a doy
#'  datetodoy(as.Date("2000-04-01"))
#'
#' @export
#' @rdname datetodoy
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#'
datetodoy <- function(dates, abort = FALSE){
  dates <- as.Date(dates)
  if (is.na(max(dates))) {
    if (abort) {
      stop("datetodoy --> some input 'dates' can not be parsed to `Date` ",
           "objects ! Aborting !" )
    } else {
      warning("datetodoy --> some input 'dates' can not be parsed to `Date` ",
              "objects ! Aborting !" )
    }
  }
  doys <- as.numeric(strftime(dates, format = '%j'))
  return(doys)
}
