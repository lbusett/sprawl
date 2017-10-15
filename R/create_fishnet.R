#' @title Create a "fishnet" vector over the extent of a spatial object
#' @description Function to create a polygon fishnet of specified resolution
#'  over a raster dataset. By default, the fishnet is built so that
#'  each cell corresponds to a raster cell. If the `pix_for_cell` argument is
#'  set, then the polygons of the fishnet are built so to include the specified
#'  number of cells in each direction. If the `cellsize` option is set, a fishnet
#'  of the specified resolution is created over the extent of the raster)
#' @param in_rast raster file or object of class `raster*` on which the fishnet
#'   needs to be derived.
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
#' @param exact_csize `logical` If TRUE, the function strictly adheres to what
#'   specified in `cellsize` or `pix_for_cell` (i.e., it does not alter the fishnet
#'   cell to get a regular fishnet over the raster). The last column/row of the
#'   fisnhet have therefore a different area, but the other cells respect what
#'   specified by the user (i.e., having a regular 2x2 km grid).
#' @param to_file `logical` PARAM_DESCRIPTION, Default: FALSE.
#' @param out_shape `logical` PARAM_DESCRIPTION, Default: FALSE.
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
#' \dontrun{
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
#'   # plotting with `exact_csize` = FALSE creates a grid that covers the extent with
#'   # regular cells, by adapting the cellsize (see `sf::st_make_grid`)
#'   fishnet  <- create_fishnet(in_rast, cellsize = c(25,25),
#'                                   exact_csize = F)
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   # using shape = "hex" gives and hexagonal grid instead
#'   fishnet  <- create_fishnet(in_rast, cellsize = c(25,25),
#'                                   exact_csize = T, shape = "hex")
#'   plot_rast(in_rast, in_poly = fishnet)
#'  }
#' @rdname create_fishnet
#' @export
#' @importFrom raster extent res
#' @importFrom sf st_as_sfc st_make_grid st_sf
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
create_fishnet <- function(in_rast,
                           pix_for_cell = 1,
                           shape        = "rect",
                           cellsize     = NULL,
                           exact_csize  = TRUE,
                           out_file     = FALSE,
                           overwrite    = TRUE,
                           crop_layer   = NULL,
                           verbose      = TRUE) {

  #TODO modify the out_shape/to_file structure. Only use "out_filename"

  call <- match.call()
  if (verbose) message("create_fishnet --> Creating Fishnet over ",
                       call[[2]])
  #   __________________________________________________________________________
  #   Check the arguments                                                   ####
  #

  in_type <- get_rastype(in_rast)
  if (in_type == "rastfile") {
    in_rast <- read_rast(in_rast)
  }

  if (is.null(cellsize)) {
    cellsize <- raster::res(in_rast) * pix_for_cell
  } else {
    if (length(cellsize) == 1) {
      cellsize <- c(cellsize, cellsize)
    }
  }

  #   __________________________________________________________________________

  in_ext <- out_ext <- get_extent(in_rast)

  if (exact_csize & shape == "rect") {
    #   extend the extent so that it contains an integer number of cells   ####
    x_range <- in_ext@extent[3] - in_ext@extent[1]
    y_range <- in_ext@extent[4] - in_ext@extent[2]

    fullcells_x <- x_range / cellsize[1]
    fullcells_y <- y_range / cellsize[2]

    if (!(fullcells_x - floor(fullcells_x)) == 0 ) {
      out_ext@extent[3] <- out_ext@extent[1] + cellsize[1] * (floor(fullcells_x) + 1) #nolint
    }

    if (!(fullcells_y - floor(fullcells_y)) == 0 ) {
      out_ext@extent[4] <- out_ext@extent[2] + cellsize[2] * (floor(fullcells_y) + 1) #nolint
    }
  }

  if (shape == "rect") {
    ext_poly <- as(out_ext, "sfc_POLYGON")
    geometry <- sf::st_make_grid(ext_poly,
                                 cellsize,
                                 what = "polygons")
    fish <- sf::st_sf(cell_id = seq_len(length(geometry)[1]),
                      geometry = geometry) %>%
      crop_vect(in_rast, verbose = verbose)
  } else {
    ext_poly <- as(out_ext , "sfc_POLYGON")
    geometry <- sf::st_make_grid(ext_poly,
                                 cellsize,
                                 what = "corners")
    hexes <- sp::spsample(as(geometry,"Spatial"),
                          type = 'hexagonal',
                          n = length(geometry), offset = c(0,0.5))
    fish <- sp::HexPoints2SpatialPolygons(hexes) %>%
      sf::st_as_sf(hexes)
  }
  if (is.null(out_file)) {
    return(fish)
  } else {
    write_shape(fish, out_file, overwrite = overwrite)
  }
}
