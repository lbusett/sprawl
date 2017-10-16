#' @title reclassify values of a raster
#' @description function to reclassify values of a raster object or raster file
#'   based on a reclassification matrix which specifies which value in the output
#'   should be taken by different **intervals** of values of the input raster
#'   (simple wrapper for the raster::reclassify function, providing a (somehow)
#'   easier and extended I/O functionality and possibility to set the class names
#'   for the new raster using `raster::ratify`
#' @param in_rast Input raster file or "R" raster layer to be reclassified
#' @param reclass_matrix `data.frame` 3 column data frame with column names equal
#'   to `start`, `end` and `new`. Each line specifies an interval of values in
#'   the original raster and the value they will assume in the output.
#'   \cr  **IMPORTANT !** Intervals in `reclass_matrix` will be considered
#'   as **CLOSED on THE LEFT and OPEN ON THE RIGHT !** - see examples !!!! -
#' @param class_names `character array` optional array of names to be assigned
#'   to the new classes (useful for plotting with `plot_rast` or `plot_rast_gg`) -
#'   see examples. If NULL, class names are assigned by converting the numeric
#'   values of the new raster to character, Default: NULL
#' @param out_file `character` Name of the output raster file. If NULL and to_file == TRUE ,
#'   the raster is saved to a file in the `R` temporary folder
#'   in "R" tempdir(), Default: NULL
#' @param out_type `character`
#'   - if == "rastobj", return a `Raster` object;
#'   - if == "filename", return the filename of the masked layer (GTiff or gdal vrt format
#'   depending on other arguments - see below)
#'   Default: "rastobj" (If an invalid string is provided, defaults to `rastobj`)
#' @param overwrite `logical` If TRUE and `out_file`` already exists, the file is overwritten,
#'   Default: FALSE
#' @param verbose `logical` if TRUE, extended processing information is sent to
#'   the console in the form of messages
#' @return * if out_type == rastobject:  `*raster` corresponding to the reclassified raster
#'   * if out_type == "rastfile": name of the file where it was saved (either corresponding to
#'   `out_file` or to the name of the temporary file created (if `out_file == NULL`))
#' @details `reclass_matrix` must be a 3-columns `data.frame` (or similar), including
#'   the `start`, `end` and `new` columns. It can be easily created by, for example:
#'
#'   ```
#'   reclass_matrix <- tribble(~start, ~end, ~new,
#'                                  0,    1,   NA, # values >=0 and < 1 will be set to NA
#'                                  1,    5,   1,  # values >=1 and < 5 will be set to 1
#'                                  5,    7,   2,  # values >=5 and < 7 will be set to 2
#'                                ...,  ..., ...,
#'                                ...,  ..., ...,
#'                                 11, 100, NA)   # values >=11 and < 100 will be set to NA
#'   ```
#'  , or:
#'
#'    ```
#'    reclass_matrix <- data.frame(start = c(0,  1, 5, ..., ..., 11),
#'                                 end   = c(1,  5, 7, ..., ..., 100),
#'                                 new   = c(NA, 1, 2, ..., ..., NA)
#'   ```
#'
#'   Note that it is __FUNDAMENTAL__ for proper functioning that ALL values of the
#'   original rasterare "covered" in `reclass_matrix`. Values not falling in any
#'   of the intervals will be kept at their original values ! (checks for this
#'   will be implemented in the future!)
#' @examples \dontrun{
#'
#' # create a raster with values equal to rows number and assign a RAT
#' # to it
#'
#'   in_rast <- raster::raster(ncol = 20, nrow = 20) %>%
#'   raster::init("row")
#'   plot_rast_gg(in_rast, rast_type = "continuous", scalebar = F)
#'
#' # build a reclassification matrix
#'
#'   reclass_matrix <- tibble::tribble(
#'                          ~start, ~end, ~new,
#'                            -Inf,   5,    1, # Values  < 5 --> 1
#'                               5,   8,    2, # Values  >=5 and < 8 --> 2
#'                               8,   12,   2, # Values  >=8 and < 12 --> 2
#'                              12,  15,   NA, # Values  >=12 and < 15 --> NA
#'                              15,  Inf,   3)  # Values >=15  --> 3
#' # reclassify and assign class names
#' out_rast <- reclass_rast(in_rast,
#'                         reclass_matrix,
#'                         class_names = c("pippo", "pluto", "paperino"))
#' out_rast
#' plot_rast_gg(out_rast, scalebar = F)
#'
#'
#' # reclassify a land cover map with N classes to a 0-1 mask, retaining classes 5
#' # and 9, putting the rest to 0 and values >= 11 to NA
#' # Open  the masking file
#'
#' # in_mask <- raster(in_maskfile)
#' # setup the reclassification matrix
#'
#' #  reclass_matrix <- tibble::tribble(
#' #                ~start, ~end, ~new,
#' #                     0,   0,   NA,
#' #                     1,   5,   0,
#' #                     5,   6,   1,
#' #                     6,   9,   1,
#' #                     9,  11,   1,
#' #                    11, 100, NA)
#'
#' # reclass_file = "/home/lb/Temp/buttami/pippo_reclass.tif"
#' # outmask = reclass_rast(in_rast,
#' #                        reclass_matrix,
#' #                        r_out = TRUE)
#' # plot_rast_gg(outmask)
#' }
#' @seealso `raster::reclassify` `raster::ratify` `categorize_rast`
#' @rdname reclass_rast
#' @export
#' @importFrom raster reclassify ratify raster
#'
reclass_rast <- function(in_rast,
                         reclass_matrix,
                         class_names = NULL,
                         out_file    = NULL,
                         out_type    = "rastobject",
                         overwrite   = FALSE,
                         verbose     = TRUE) {

  call <- match.call()
  if (verbose) message("reclass_rast --> Reclassifying: ",
                       as.character(call[[2]]), " on values of: ",
                       as.character(call[[3]]))
  #   __________________________________________________________________________
  #   determine the required data type based on maximum value of the output ####
  #   raster

  max_out <- max(reclass_matrix$new, na.rm = TRUE)
  if (max_out <= 255) {
    ot <- "INT1U"
  }  else  {
    if (max_out <= 65536) {
      ot <- "INT2S"
    } else {
      ot <- "INT4S"
    }
  }

  # ___________________________________________________________________________
  # Launch raster::reclassify, using intervals open on the left and closed ####
  # on the right, then apply raster::ratify to initialize the RAT on the
  # result
  if (is.null(out_file)) {
    out_file <- tempfile(fileext = ".tif")
  }

  recl_rast <- raster::reclassify(in_rast,
                                  reclass_matrix,
                                  filename       = out_file,
                                  include.lowest = TRUE,
                                  right          = FALSE,
                                  overwrite      = overwrite,
                                  datatype       = ot) %>%
    raster::ratify() %>%
    categorize_rast(class_names, verbose = FALSE)


  if (out_type == "rastobject") {
    return(recl_rast)
  } else {
    return(read_rast(out_file))
  }
}
