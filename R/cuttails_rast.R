#' @title remove outliers from a raster image
#' @description the function allows to automatically set to NoData eventual outliers present in a
#'   raster single- or multi-band object (or file). For each band, quantiles of the distribution
#'   of pixel values are computed using `raster::quantile`, and the values falling outside the
#'   limits set with the `tails` argument are set to NoData in the output
#' @param in_rast `*raster` object (single layer or stack), or filename of a raster file
#' @param tails `numeric array (2)` quantiles of the distribution of pixel values used for the cut.
#'  For example, setting tails = c(0.05, 0.95) will set to NoData all pixels below the 5th and
#'  above the 95th percentile,  Default: c(0.02, 0.98)
#' @param to_file DESCRIPTION
#' @param verbose DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  r     <- raster::raster(ncol=100, nrow=100)
#'  r     <- raster::setValues(r, 1:10000)
#'  s     <- raster::brick(r,r,r)
#'  summary(s)
#'  s_cut <- cuttails_rast(s)
#'  summary(s_cut)
#'  s_cutb1 <- cuttails_rast(s[[1]])
#'  summary(s_cutb1)
#'  }
#' }
#' @seealso
#'  \code{\link[raster]{quantile}},\code{\link[raster]{getValues}},\code{\link[raster]{setValues}},\code{\link[raster]{brick}}
#' @rdname cuttails_rast
#' @export
#' @importFrom raster quantile getValues setValues brick
#' @importFrom data.table data.table
#'
cuttails_rast <- function(in_rast,
                          tails   = c(0.02,0.98),
                          to_file = FALSE,
                          verbose = TRUE) {

  quantiles <- raster::quantile(in_rast, probs = tails, na.rm = T)
  nbands    <- length(quantiles)/2
  names_or  <- names(in_rast)
  if (nbands > 1) {
    for (band in 1:dim(quantiles)[1]) {
      band_values     <- data.table::data.table(V1 = raster::getValues(in_rast[[band]]))
      band_values[((V1 < quantiles[band,][1]) | (V1 > quantiles[band,][2])), V1 := NA]
      in_rast[[band]] <- raster::setValues(in_rast[[band]], band_values[,V1])
    }
  } else {
    # browser()
    band_values <- data.table::data.table(V1 = raster::getValues(in_rast))
    band_values[((V1 < quantiles[1]) | (V1 > quantiles[2])), V1 := NA]
    in_rast     <- raster::setValues(in_rast, band_values[,V1])
  }
  names(in_rast) <- names_or
  return(raster::brick(in_rast))
}









