#' @title Assign labels and plotting colors to a categorical raster
#' @description  Assign labels and plotting colors to a categorical raster
#' @param in_rast a categorical `Raster` object or the path to a valid
#'   categorical raster file
#' @param class_names `character array` optional array of names to be assigned
#'   to the raster classes (useful for plotting with `plot_rast` or `plot_rast_gg`) -
#'   see examples. Order of the class names should follow the numeric sortuing
#'   of the values to which class names should correspond. If NULL, class names
#'   are assigned by converting the numeric values of the new raster to character,
#'   Default: NULL
#' @param class_colors `character array` optional array of colors to be assigned
#'   to the rasterclasses (useful for plotting with `plot_rast` or `plot_rast_gg`) -
#'   see examples. Order of the class names should follow the numeric sortuing
#'   of the values to which class colors should correspond. If NULL, class colors
#'   are assigned automatically using `scales::hue_pal()(n_class)`, Default: NULL
#' @param verbose `logical` if TRUE, extended processing information is sent to
#'   the console in the form of messages
#' @return a 'Raster' object equal to `in_rast` to which a Raster Attribute table
#'   has been added (see `raster::ratify`)
#' @details Simple wrapper around `raster::ratify`, providing the added
#'   functionality of specifying specific names for the classes.
#' @examples
#'
#'   library(raster)
#'   library(dplyr)
#'   in_rast <- raster::raster(ncol = 5, nrow = 5) %>%
#'     raster::init("row")
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
#'
#'   #'   # categorize and assign custom class names and colors
#'   cat_rast <- set_rastlabels(in_rast,
#'                               class_names = c("Water", "Land",
#'                               "Vegetation", "Urban", "Other"),
#'                               class_colors = c("blue", "maroon",
#'                               "green", "red", "black"))
#'   levels(cat_rast)[[1]][1]
#'
#'   # Class names are automatically recognized by [`plot_rast_gg`]
#'   plot_rast_gg(cat_rast, scalebar = FALSE)
#'
#' @rdname set_rastlabels
#' @seealso `raster::ratify`
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster nlayers ratify
#' @importFrom scales hue_pal
#'
set_rastlabels <- function(in_rast,
                           class_names  = NULL,
                           class_colors = NULL,
                           verbose      = TRUE) {

  call <- match.call()
  if (verbose) message("set_rastlabels --> categorizing ", call[[2]],
                       ifelse(length(class_names != 0),
                              paste(" and assigning class names based on ",
                                    call[[3]]), ""))
  #   __________________________________________________________________________
  #   build the RAT for the new raster. Use class_names and colors          ####
  #   if available
  in_rast <- cast_rast(in_rast, "rastobject")
  if (raster::nlayers(in_rast) != 1) {
    stop("set_rastclasses --> the function currently support only single-band
         rasters. Aborting!")
  }
  cat_rast <- raster::ratify(in_rast)
  n_class  <- dim(cat_rast@data@attributes[[1]])[1]

  if (length(class_names) != n_class) {
    warning("set_rastlabels --> Provided class_names do not match the number ",
            "of classes of ", call[[2]], ". \n Class names will be ignored!")
    class_names = NULL
  }



  if (is.null(class_names)) {
    cat_rast@data@attributes[[1]]$Class <-
      as.character(cat_rast@data@attributes[[1]]$ID)
  } else {
    cat_rast@data@attributes[[1]]$Class <- class_names
  }

  if (is.null(class_colors)) {
    cat_rast@data@attributes[[1]]$Color = scales::hue_pal()(n_class)
  } else {
    cat_rast@data@attributes[[1]]$Color = class_colors
  }

  return(cat_rast)

}
