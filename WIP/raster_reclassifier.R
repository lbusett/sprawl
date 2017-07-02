#' rast_reclass
#'
#' @param in_rast Input raster file or "R" raster layer to be reclassified
#' @param rcl_mat input reclassification matrix. list of list created  as follows:
#'    rcl_mat <- list(
#'       list(start = X,  end  = Y , new = n1),
#'       list(start = X1, end  = Y1, new = n2),
#'       .....,
#'       list(start = Xn, end = Yn, new = nn)
#'    )
#'    IMPORTANT ! Intervals are CLOSED on THE LEFT and OPEN ON THE RIGHT !
#'    - see examples !!!! -
# Therefore, start = 1, end = 5 means from 1 to 4.999999, etcetera !)
#' @param out_rast name of the output raster file
#' @param r_out    logical flag indicating if the output raster should be sent back to "R"
#' @param ovr      logical indicateing if the output file should be overwritten if already existing
#'
#' @return if (r_out == TRUE), returns a "R" raster object corresponding to the reclassified raster
#' @export
#'
#' @importFrom raster reclassify raster
#' @importFrom data.table rbindlist
#'
#' @examples \dontrun{
#' # reclassify a land cover map with N classes to a 0-1 mask, retaining classes 5 and 9, putting
#' # the rest to 0 and values >= 11 to NA
#' # Open  the masking file
#' in_mask <- raster(in_maskfile)
#' # setup the reclassification matrix
#' rcl_mat <- list(
#'    list(start = 0, end  = 0, new = NA),
#'    list(start = 1, end  = 5, new = 0),
#'    list(start = 5, end  = 6, new = 1),
#'    list(start = 6, end  = 9, new = 1),
#'    list(start = 9, end  = 11, new = 1),
#'    list(start = 11, end = 100, new = NA)
#'   )
#'
#'
#' reclass_file = "/home/lb/Temp/buttami/pippo_reclass.tif"
#' outmask = raster_reclassifier(in_mask, rcl_mat, out_rast = reclass_file,
#'                                 r_out = TRUE, ovr = TRUE)
#' summary(outmask)
#' }

rast_reclass <- function(in_rast, rcl_mat, out_rast, r_out = FALSE, ovr = FALSE){

  # reformat the hash table to the format wanted by "reclassify"
  rclmat  <- data.table::rbindlist(rcl_mat)
  max_out <- max(rclmat$new, na.rm = TRUE)
  if (max_out <= 255) {ot = "INT1U"}  else
  {
    if (max_out <= 65536) {
      ot = "INT2S"} else {ot = "INT4S" }
  }
  raster::reclassify(in_rast, rclmat, filename = out_rast,
                     include.lowest = TRUE, right = FALSE, overwrite = ovr,
                     datatype = ot)
  if (r_out == TRUE) {
    return(raster(out_rast))
  }

}
