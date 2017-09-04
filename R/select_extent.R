#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_object PARAM_DESCRIPTION, Default: NULL
#' @param transparency PARAM_DESCRIPTION, Default: 0.5
#' @return OUTPUT_DESCRIPTION
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  extent    <- select_extent()
#'  }
#' }
#' @export
#' @rdname select_extent
#' @importFrom checkmate assert checkClass check_null
#' @importFrom mapedit editMap
#' @importFrom mapview viewExtent mapview
#' @importFrom raster raster
#' @importFrom sf st_dimension
#' @importFrom wrapr "%.>%"
select_extent <- function(in_object = NULL,
                          transparency = 0.5) {

  . <- NULL
  #  _________________________________ _________________________________________
  #  Create dummy dataset with global extenction if in_object = NULL        ####
  if (is.null(in_object)) {
    in_object <- raster::raster(nrows = 18, ncols = 36, vals  = 0) %.>%
      get_extent(.) %.>%
      as(., "sfc_POLYGON")
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
    mapview::mapview(in_object, alpha.regions = 1 - transparency)
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
        " Please select at least two points, a rectengle or a polygon. ",
        "Aborting!"
      )
    }
    get_extent(extent$finished)
  }
}
