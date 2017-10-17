#' @title categorize a raster and assign class names
#' @description FUNCTION_DESCRIPTION
#' @param in_rast a `Raster` object or the path to a valid rster file
#' @param class_names `character array` optional array of names to be assigned
#'   to the new classes (useful for plotting with `plot_rast` or `plot_rast_gg`) -
#'   see examples. Order of the class names should follow the numeric sortuing
#'   of the values to which class names should correspond. If NULL, class names
#'   are assigned by converting the numeric values of the new raster to character,
#'   Default: NULL
#' @param verbose `logical` if TRUE, extended processing information is sent to
#'   the console in the form of messages
#' @return a 'Raster' object equal to `in_rast` to which a Raster Attribute table
#'   has been added (see `raster::ratify`)
#' @details Simple wrapper around `raster::ratify`, providing the added
#'   functionality of specifying specific names for the classes.
#' @examples
#' \dontrun{
#'   in_rast <- raster::raster(ncol = 5, nrow = 5) %>%
#'     init("row")
#'   levels(in_rast)[[1]]
#'
#'   # categorize and assign standard class names
#'   cat_rast <- set_rastlabels(in_rast)
#'   levels(cat_rast)[[1]]
#'
#'   # categorize and assign custom class names
#'   cat_rast <- set_rastlabels(in_rast,
#'                               class_names = c("Water", "Land",
#'                               "Vegetation", "Urban", "Other"))
#'   levels(cat_rast)[[1]]
#'   plot_rast_gg(cat_rast, leg_labels = levels(cat_rast)[[1]]$Class,
#'                scalebar = FALSE)
#'  }
#' @rdname set_rastlabels
#' @seealso `raster::ratify`
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster nlayers ratify
set_rastlabels <- function(in_rast,
                            class_names = NULL,
                            verbose     = TRUE) {

  call <- match.call()
  if (verbose) message("set_rastlabels --> categorizing ", call[[2]],
                       ifelse(length(class_names != 0),
                              paste(" and assigning class names based on ",
                                    call[[3]]), ""))
  #   __________________________________________________________________________
  #   build the RAT for the new rastrer. Use class_names if available       ####
  in_rast <- cast_rast(in_rast, "rastobject")
  if (raster::nlayers(in_rast) != 1) {
    stop("set_rastclasses --> the function currently support only single-band
         rasters. Aborting!")
  }
  cat_rast <- raster::ratify(in_rast)
  n_class  <- dim(cat_rast@data@attributes[[1]])[1]

  if (length(class_names) != n_class & length(class_names != 0)) {

    warning("The number of specified class names is not equal to the ",
            "number of classes in the reclassified raster. \nClass names will ",
            "be set to numeric values equal to the raster value")
    class_names <- NULL
  }

  if (is.null(class_names)) {
    cat_rast@data@attributes[[1]]$Class = as.character(
      cat_rast@data@attributes[[1]]$ID
    )
  } else {
    cat_rast@data@attributes[[1]]$Class = class_names
  }

  return(cat_rast)

}
