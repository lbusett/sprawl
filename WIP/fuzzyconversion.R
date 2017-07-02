#' lb_fuzzyconversion
#'
#' @param in_raster input raster file. Multiband is allowed
#' @param thresholds thresholds to be used for fuzzyfication. Input raster is converted to a 0:1 fuzzy score
#' according to the specified thresholds,
#' @param ... any other parameter passable to "calibrate"
#'
#' @return raster object in 0:1 range obtained applying a fuzzification on the input values according to specified thresholds.
#' See help for "calibrate" function in QCA library for details.
#' @export
#'

#'

lb_fuzzyconversion = function(in_raster, thresholds , ...) {
  out_raster = stack(in_raster)
  values(out_raster) = calibrate(values(out_raster), thresholds = thresholds, type = "fuzzy")
  out_raster
}

