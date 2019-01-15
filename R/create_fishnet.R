#' @title Create a "fishnet" vector over the extent of a spatial object
#' @description Function to create a polygon fishnet of specified resolution
#'  over a raster dataset. By default, the fishnet is built so that
#'  each cell corresponds to a raster cell. If the `pix_for_cell` argument is
#'  set, then the polygons of the fishnet are built so to include the specified
#'  number of cells in each direction. If the `cellsize` option is set, a fishnet
#'  of the specified resolution is created over the extent of the raster)
#' @param in_obj raster/vector file or object, or sprawlext object on which extent
#'   the fishnet should be built
#' @param pix_for_cell `numeric(1/2)` (optional) 1/2 element numeric array
#'   specifying how many pixels of the #' raster will be included in each
#'   polygon of the fishnet (if only one element is provided, the #' same number
#'   of pixel is aggregated in each direction). Ignored if `cellsize` is not null,
#'   Default: 1
#' @param shape `character [\"rect\" \"hex\"`]
#' @param cellsize `numeric(1/2)` (optional) 1/2 element array specifying the
#'   dimensions of the desired #' cells in the x and y directions (if only one
#'   element is provided, the same cellsize is used in each direction),
#'   Default: NULL
#' @param out_type `character [`sf`, `sfc`]` if `sf`, return an `sf` POLYGON
#'  object, with a `cell_id` column. If `sfc` return only the geometry, default: `sf`
#' @param out_file `logical` PARAM_DESCRIPTION, Default: FALSE.
#' @param overwrite `logical` PARAM_DESCRIPTION, Default: TRUE.
#' @param crop_layer `logical` object of class `Extent`. If not null, the
#'   fishnet is cropped on this extent, without "moving" the nodes of the grid.
#'   This is useful to crop a grid created on the basis of a different raster
#'   coordinates on top of a different raster, Default: FALSE. CURRENTLY NOT
#'   IMPLEMENTED - MAY BE DEPRECATED !
#' @param verbose `logical` If FALSE, processing messages are suppressed,
#'   Default: TRUE.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'
#'   # create a fishnet over an input raster file and save it as a shapefile
#'   library(raster)
#'   library(sprawl)
#'   library(rasterVis)
#'
#'   in_rast  <- build_testraster(20,40,1)
#'   fishnet  <- create_fishnet(in_rast)
#'
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   fishnet  <- create_fishnet(in_rast, pix_for_cell = c(4,2))
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   fishnet  <- create_fishnet(in_rast, cellsize = c(25,25))
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   # using shape = "hex" gives and hexagonal grid instead
#'   fishnet  <- create_fishnet(in_rast, cellsize = c(25,25),
#'                                   exact_csize = TRUE, shape = "hex")
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#' @rdname create_fishnet
#' @export
#' @importFrom raster extent res
#' @importFrom sf st_as_sfc st_make_grid st_sf
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
create_fishnet <- function(in_obj,
                           pix_for_cell = 1,
                           shape        = "rect",
                           cellsize     = NULL,
                           exact_csize  = TRUE,
                           out_file     = NULL,
                           out_type     = "sf",
                           overwrite    = TRUE,
                           crop_layer   = NULL,
                           verbose      = TRUE) {


  call <- match.call()
  if (verbose) message("create_fishnet --> Creating Fishnet over ",
                       call[[2]])
  #   __________________________________________________________________________
  #   Check the arguments                                                   ####
  #
  assertthat::assert_that(out_type %in% c("sf", "sfc"))

  in_ext <- out_ext <- get_extent(in_obj)

  # check if `in_obj` is a raster. If so, and cellsize is notset, use
  # pix_for_cell instead

  if (inherits(in_obj, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    if (is.null(cellsize)) {
      cellsize <- raster::res(in_obj) * pix_for_cell
    } else {
      if (length(cellsize) == 1) {
        cellsize <- c(cellsize, cellsize)
      }
    }
  }

  if (shape == "rect") {
    ext_poly <- sf::st_as_sfc(sf::st_bbox(in_ext@extent,
                              crs = in_ext@proj4string))
    geometry <- sf::st_make_grid(ext_poly,
                                 cellsize,
                                 what = "polygons")
    fish <- sf::st_sf(cell_id = seq_len(length(geometry)[1]),
                      geometry = geometry) %>%
      crop_vect(., in_obj, verbose = verbose)
  } else {
    ext_poly <- sf::st_as_sfc(sf::st_bbox(in_obj))
    geometry <- sf::st_make_grid(ext_poly,
                                 cellsize, square = FALSE)
  }
  if (is.null(out_file)) {
    return(fish)
  } else {
    write_shape(fish, out_file, overwrite = overwrite)
  }
}
