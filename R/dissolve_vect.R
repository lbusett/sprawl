#' @title dissolve attributes of a vector and aggregate its table
#' @description Perform a "dissolve" (a.k.a. aggregation) operation on a vector
#'   object according to a grouping variable, also aggregate values of the
#'   attribute table for all other columns over the same grouping variable.
#'
#'   Aggregation of factor or character columns is done as follows:
#'     - If all polygons that are aggregated to a common dissolved polygon had
#'       the same value for the column, then the value is preserved;
#'     - If polygons had different values, the column takes the value "variabl"
#'       or NA (depending on `var_as_NA` )
#'
#'   Aggregation of factor or character columns is done as follows:
#'
#'     - If the column represent a "quantity" (e.g., Population), the value for each
#'       dissolved is computed as the sum of the values of the original
#'       polygons belonging to it;
#'TODO
#'     - If the column represents a "density" (e.g., ton ha-1), the value for each
#'       dissolved is computed as the average of the values of the original
#'       polygons belonging to it, weighted by their area;  STILL TO BE IMPLEMENTED !
#'       CURRENTLTY ALL COLUMNS ARE COMPUTED SIMPLY aS THE SUM !!!!
#'
#' @param in_vect Either a `R` vector object (`sf` or `sp`), or the path to
#'   a valid vector file
#' @param dissolve_var `character` column of the attribute table of
#'   `in_vect` on to which the dissolve has to be performed.
#' @param var_as_NA `logical` TO BE DESCRIBED - see examples for now!!!,
#'  Default: FALSE
#' @param out_file  `character` output file name
#' @param out_type `character ["vectfile" | "vectobject"]` If "vectfile", and `out_file`
#'   is not NULL, the function returns the name of the saved shapefile. Otherwise,
#'   it returns the reprojected vector object, with the format specified by
#'   out_class,  Default: 'vectobject'
#' @param overwrite `logical` if TRUE, output file will be overwritten if existing,
#'   Default: FALSE
#' @param verbose `logical` if TRUE, provide messages on processing, Default: TRUE
#' @return an `*sf` object. The object is also saved to disk as a shapefile if
#'   `out_file` is provided
#' @examples
#' \dontrun{
#'  library(sprawl.data)
#'
#'  indata    <- read_vect(system.file("extdata/shapes","poly_lomb.shp",
#'     package = "sprawl.data"))
#'  plot_vect(in_vect, fill_var = "NAME_2")
#'  plot_vect(in_vect, fill_var = "Population", palette_name = "RdYlGn")
#'
#'  # Dissolve the vector to provinces
#'  diss <- dissolve_vect(in_vect, "NAME_2")
#'  diss
#'
#'  plot_vect(diss, fill_var = "NAME_2")
#'  provinces <- get_boundaries("ITA", 2)
#'
#'  plot_vect(diss, fill_var = "Population", palette_name = "RdYlBu",
#'          borders_layer = provinces, borders_txt_field = "NAME_2",
#'          leg_position = "bottom")
#'
#'  }
#' @rdname dissolve_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that is.writeable
#' @importFrom dplyr group_by_ summarize_all first mutate_if select_if n_distinct summarize_if left_join nth
#' @importFrom sf st_cast st_set_geometry st_as_sf
#' @importFrom tibble as_data_frame


dissolve_vect <- function(in_vect,
                          dissolve_var,
                          var_as_NA = FALSE,
                          out_file  = NULL,
                          out_type  = "vectobject",
                          overwrite = FALSE,
                          verbose   = TRUE
                          ) {

  #   ____________________________________________________________________________
  #   Check the inputs                                                        ####

  #TODO find a workaround to avoid this !!!!
  library("sf")
  in_vect <- cast_vect(in_vect, "sfobject")
  if (!(dissolve_var %in% names(in_vect))) stop(
    "The selected grouping variable is not present in the columns of the input ", #nolint
    "object. Aborting !")
  #   ____________________________________________________________________________
  #   Perform the dissolve                                                   ####
  #

  # Chars and factors
  out_object_fact <- in_vect %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_cast("GEOMETRYCOLLECTION") %>%
    dplyr::group_by_(dissolve_var) %>%
    dplyr::summarize_all(first) %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::select_if(is.factor)

  #   ____________________________________________________________________________
  #   Check the non-grouping columns. If in the input object the different    ####
  #   features that are joined in each group of the output contain different
  #   values, the value of the column in the output is set to "variable".
  #
  check_cols <- tibble::as_data_frame(in_vect) %>%
    dplyr::group_by_(dissolve_var) %>%
    dplyr::summarize_all(n_distinct)

  for (selcol in 2:(length(out_object_fact) - 1)) {

    pos <- which(names(check_cols) == names(out_object_fact)[selcol])
    out_object_fact[selcol] <- ifelse((max(check_cols[[pos]]) != 1),
                                      ifelse(var_as_NA, NA, "variable"),
                                      out_object_fact[,selcol] )
  }

  # Numeric: Sum (TODO implement distinction between sums and densities !)
  out_object_num <- in_vect %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by_(dissolve_var) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = T)

  if (length(out_object_num) != 0 ) {
    out_object <- dplyr::left_join(out_object_fact,
                                   out_object_num)
  }


  out_object <- out_object %>%
    sf::st_as_sf()

  # save to file if needed

  if (!is.null(out_file)) {
    if (!dir.exists(dirname(out_file))) {
      make_folder(out_file,
                  type = "filename",
                  verbose = FALSE)
    }
    assertthat::assert_that(
      assertthat::is.writeable(dirname(out_file)),
      msg = strwrap(paste0("reproj_vect --> ", call[[4]],
                           " is not a valid or writeable file. Aborting!"))
    )
    write_shape(out_object, out_file, overwrite = overwrite)

    if (out_type == "vectfile") return(out_file) else return(out_object)

  } else {
    return(out_object)
  }
}

