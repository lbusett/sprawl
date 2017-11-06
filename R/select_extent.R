#' @title Interactively select an extent
#' @description Select an extent interactively from a leaflet map
#' @param in_object a `R` spatial object of any kind supported by
#'   `mapview` ("Raster", "sp", "sf") to be shown as reference on the map
#'   (see examples). If NULL, an empty map of the world is shown, Default: NULL
#' @param reproject `logical` If TRUE, and a valid `in_object` was provided,
#'   the selected extent is returned in the same CRS of `in_object`, otherwise
#'   it is returned in CRS 4326
#' @param transparency PARAM_DESCRIPTION, Default: 0.5
#' @return a `sprawlext` object representing the selected extent.
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @details DETAILS
#' @examples \donttun{
#' if(interactive()){
#'  in_rast <- read_rast(system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data"))
#'  extent    <- select_extent(in_rast)
#'  extent
#'  }
#'  }
#' @export
#' @rdname select_extent
#' @importFrom checkmate assert checkClass check_null
#' @importFrom mapedit editMap
#' @importFrom mapview viewExtent mapview
#' @importFrom raster raster
#' @importFrom sf st_dimension
#' @importFrom wrapr "%.>%"
select_extent <- function(in_object = NULL,
                          reproject = TRUE,
                          transparency = 0.5) {

  . <- NULL
  #  _________________________________ _________________________________________
  #  Create dummy dataset with global extenction if in_object = NULL        ####
  #  and reset transparency to 1
  if (is.null(in_object)) {
    in_object <- raster::raster(nrows = 18, ncols = 36, vals  = 0) %.>%
      get_extent(.) %.>%
      as(., "sfc_POLYGON")
    transparency = 1
  }
  #   ________________________________________________________________________
  #   Check arguments                                                     ####

  checkmate::assert(
    checkClass(in_object, "Raster"),
    checkClass(in_object, "sf"),
    checkClass(in_object, "sfc"),
    checkClass(in_object, "sp"),
    checkClass(in_object, "sprawlext"),
    check_null(in_object)
  )

  #   ________________________________________________________________________
  #   Realize selection interface                                         ####
  extent    <- mapedit::editMap(
    mapview::mapview(in_object, alpha.regions = 1 - transparency),
    viewer = shiny::dialogViewer("Select an Extent", width = 600, height = 600)
  )

  #   ________________________________________________________________________
  #   Return a sprawlext object                                           ####
  if (is.null(extent)) {
    message("select_extent --> Selection aborted. Returning NULL")
    return(NULL)
  } else {
    if (sf::st_dimension(extent$finished) == 0 & dim(extent$finished)[1] == 1) {
      stop(
        "get_extent --> Impossible to derive an extent from a single point.",
        " Please select at least two points, a rectangle or a polygon. ",
        "Aborting!"
      )
    }

    out_ext <- get_extent(extent$finished)
    if (reproject) out_ext <- reproj_extent(out_ext, get_proj4string(in_object))
    out_ext
  }
}
