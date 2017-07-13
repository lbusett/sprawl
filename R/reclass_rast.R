#' reclass_rast
#'
#' @title reclass_rast
#' @description FUNCTION_DESCRIPTION
#' @param in_rast Input raster file or "R" raster layer to be reclassified
#' @param rcl_mat `data.frame` 3 column data frame with column names equal to `start`, `end` and `new`
#' Each line specifies an intervals of values in the original raster and the value they will
#' assume in the output. Can be created for example as follows:
#'  rcl_mat <- tibble::tribble(
#'                 ~start, ~end, ~new,
#'                      0,   0,   NA,   # --> 0 in input goes to NA in output
#'                      1,   4,   1,    # --> values from 1 to 4 (excluded) go to 1 in output
#'                   ...,  ..., ...,
#'                   ...,  ..., ...,
#'                      9,  11,   0,   # --> values from 9 to 11 (excluded) go to 0 in output
#'                      11, 100, NA)   # --> everything equal/above 11 go to NA in output
#'
#'    **IMPORTANT ! Intervals are CLOSED on THE LEFT and OPEN ON THE RIGHT !** - see examples !!!! -
#' @param out_rast `character` Name of the output raster file. If NULL, a temporary raster is created
#' in "R" tempdir(), Default: NULL
#' @param r_out `logical` If TRUE, the reclassified raster is returned to the caller, Default: TRUE
#' @param overwrite `logical` If TRUE and `out_rast`` already exists, the file is overwritten,
#' Default: FALSE
#' @return if (r_out == TRUE), "R" `raster` object corresponding to the reclassified raster,
#' otherwise  the name of the file where it was saved (either corresponfing to `out_rast` or to the
#' name of thetemporary file created (if `out_rast == NULL`))
#' @details this is a simple wrapper for the [raster::reclassify] function, providing somehow easier
#' use and extended I/O functionality
##' @examples \dontrun{
#' # reclassify a land cover map with N classes to a 0-1 mask, retaining classes 5 and 9, putting
#' # the rest to 0 and values >= 11 to NA
#' # Open  the masking file
#' in_mask <- raster(in_maskfile)
#' # setup the reclassification matrix
#'
#'  rcl_mat <- tibble::tribble(
#'                 ~start, ~end, ~new,
#'                      0,   0,   NA,
#'                      1,   5,   0,
#'                      5,   6,   1,
#'                      6,   9,   1,
#'                      9,  11,   1,
#'                      11, 100, NA)
#'
#' reclass_file = "/home/lb/Temp/buttami/pippo_reclass.tif"
#' outmask = reclass_rast(in_rast,
#'                        rcl_mat,
#'                        r_out = TRUE)
#' plot(outmask)
#' }
#' @seealso
#'  \code{\link[raster]{reclassify}}
#' @rdname reclass_rast
#' @export
#' @importFrom raster reclassify
#'
reclass_rast <- function(in_rast,
                         rcl_mat,
                         out_rast  = NULL,
                         r_out     = TRUE,
                         overwrite = FALSE){


  #   ____________________________________________________________________________
  #   determine the required data type based on maximum value of the output   ####
  #   raster

  max_out <- max(rcl_mat$new, na.rm = TRUE)
  if (max_out <= 255) {
    ot = "INT1U"
  }  else  {
    if (max_out <= 65536) {
      ot = "INT2S"
    } else {
      ot = "INT4S"
    }
  }

  #   ____________________________________________________________________________
  #   Launch raster::reclassify, using intervals open on the left and closed o####
  #   on the right
  if (is.null(out_rast)) {
    out_rast <- tempfile(fileext = ".tif")
  }
  raster::reclassify(in_rast,
                     rcl_mat,
                     filename       = out_rast,
                     include.lowest = TRUE,
                     right          = FALSE,
                     overwrite      = overwrite,
                     datatype       = ot)
  if (r_out == TRUE) {
    return(raster(out_rast))
  } else {
    return(out_rast)
  }
}
