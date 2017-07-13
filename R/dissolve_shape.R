#' @title dissolve_shape
#' @description FUNCTION_DESCRIPTION
#' @param in_object PARAM_DESCRIPTION
#' @param byvar PARAM_DESCRIPTION
#' @param var_as_NA PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  indata    <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl"))
#'  byvar     <- "category"
#'  out_shape <- dissolve_shape(indata, byvar)
#'  out_shape
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{as_data_frame}}
#' @rdname dissolve_shape
#' @export
#' @importFrom dplyr group_by_ summarize_all first n_distinct
#' @importFrom tibble as_data_frame

dissolve_shape <- function(in_object, byvar, var_as_NA = FALSE) {


#   ____________________________________________________________________________
#   Check the inputs                                                        ####

  type <- check_spatype(in_object)
  if (type == "spobject") in_object <- as(in_object, "Spatial")
  if (type == "vectfile") in_object <- read_shape(in_object, strigsAsFactors = TRUE)
  if (type == "none") stop("Input object is not a valid `*sp` or *`sf` object. Aborting !")
  if (!(byvar %in% names(in_object))) stop("select grouping variable is not present in the columns of the
                                  input object. Aborting !")
#   ____________________________________________________________________________
#   Perform the dissolve                                                   ####
#
  out_object <- in_object %>%
    # sf::st_cast("MULTIPOLYGON") %>%
    dplyr::group_by_(byvar) %>%
    dplyr::summarize_all(dplyr::first) %>%
    mutate_if(is.character, factor)

#   ____________________________________________________________________________
#   Check the non-grouping columns. If in the input object the different features ####
#   that are joined in each group of the output contain different values, the
#   value of the column in the output is set to "variable".
#
  check_cols <- tibble::as_data_frame(in_object) %>%
    dplyr::group_by_(byvar) %>%
    dplyr::summarize_all(dplyr::n_distinct)

  for (selcol in 2:(length(check_cols) - 1)) {
    out_object[,selcol] = ifelse((max(check_cols[[selcol]]) != 1),
        ifelse(var_as_NA, NA, "variable"),
        out_object[,selcol] )
  }

  return(out_object)
}

