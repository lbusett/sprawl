#' @title reclassify values of a raster
#' @description function to reclassify values of a raster object or raster file based on a
#'   reclassification matrix which specifies which value in the output should be taken by different
#'   **intervals** of values of the input raster (simple wrapper for the
#'   raster::reclassify function, providing a (somehow) easier and extended I/O functionality)
#' @param in_rast Input raster file or "R" raster layer to be reclassified
#' @param reclass_matrix `data.frame` 3 column data frame with column names equal to `start`, `end` and `new`
#' Each line specifies an intervals of values in the original raster and the value they will
#' assume in the output. See Details. \cr  **IMPORTANT !** Intervals in `reclass_matrix` will be considered
#' as **CLOSED on THE LEFT and OPEN ON THE RIGHT !** - see examples !!!! -
#' @param to_file `logical` If TRUE, the reclassified raster is saved to a tiff file and its
#'   name is returned to the caller, Default: TRUE
#' @param out_rast `character` Name of the output raster file. If NULL and to_file == TRUE ,
#'   the raster is saved to a file in the `R` temporary folder
#' in "R" tempdir(), Default: NULL
#' @param overwrite `logical` If TRUE and `out_rast`` already exists, the file is overwritten,
#' Default: FALSE
#' @return * if to_file == FALSE:  `*raster` corresponding to the reclassified raster
#'* if to_file == TRUE: name of the file where it was saved (either corresponding to `out_rast`
#'   or to the name of the temporary file created (if `out_rast == NULL`))
#' @details `reclass_matrix` must be a 3-columns `data.frame` (or similar), including the `start`, `end` and
#'`new` columns. It can be easily created by, for example:
#'
#'  ```
#'  reclass_matrix <- tribble(~start, ~end, ~new,
#'                                 0,    1,   NA, # values >=0 and < 1 will be set to NA
#'                                 1,    5,   1,  # values >=1 and < 5 will be set to 1
#'                                 5,    7,   2,  # values >=5 and < 7 will be set to 2
#'                               ...,  ..., ...,
#'                               ...,  ..., ...,
#'                                11, 100, NA)   # values >=11 and < 100 will be set to NA
#'  ```
#'  , or:
#'
#'  ```
#'  reclass_matrix <- data.frame(start = c(0,  1, 5, ..., ..., 11),
#'                               end   = c(1,  5, 7, ..., ..., 100),
#'                               new   = c(NA, 1, 2, ..., ..., NA)
#' ```
#' @examples \dontrun{
#' # reclassify a land cover map with N classes to a 0-1 mask, retaining classes 5 and 9, putting
#' # the rest to 0 and values >= 11 to NA
#' # Open  the masking file
#' in_mask <- raster(in_maskfile)
#' # setup the reclassification matrix
#'
#'  reclass_matrix <- tibble::tribble(
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
#'                        reclass_matrix,
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
                         reclass_matrix,
                         to_file   = FALSE,
                         out_rast  = NULL,
                         overwrite = FALSE){

  #   ____________________________________________________________________________
  #   determine the required data type based on maximum value of the output   ####
  #   raster

  max_out <- max(reclass_matrix$new, na.rm = TRUE)
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
                     reclass_matrix,
                     filename       = out_rast,
                     include.lowest = TRUE,
                     right          = FALSE,
                     overwrite      = overwrite,
                     datatype       = ot)
  if (to_file == FALSE) {
    return(raster(out_rast))
  } else {
    return(out_rast)
  }
}
