#' @title create a "fishnet" vector based on cells of a raster
#' @description function to create a raster and/or polygon fishnet of specified resolution over a
#' raster dataset. By default, the fishnet is built so that each cell corresponds to a raster cell.
#' If the `pix_for_cell` argument is set, then the polygons of the fishnet are built so to include
#' the specified number of cells in each direction. If the `cellsize` option is set, a fishnet of the
#' specified resolution is created over the extent of the raster)
#' @param in_rast raster file or object of class `raster*` on which the fishnet needs to be derived.
#' @param pix_for_cell `numeric` (optional) 1/2 element numeric array specifying how many pixels of the #' raster will be included in each polygon of the fishnet (if only one element is provided, the #' same number of pixel is aggregated in each direction). Ignored if `cellsize` is not null, #' Default: 1
#' @param cellsize `numeric` (optional) 1/2 element array specifying the dimensions of the desired #' cells in the x and y directions (if only one element is provided, the same cellsize is #' used in each direction), Default: NULL
#' @param to_file PARAM_DESCRIPTION, Default: NULL
#' @param out_shape PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: TRUE
#' @param crop_layer (optional) object of class `Extent`. If not null, the fishnet iscropped on #' this extent, without "moving" the nodes of the grid. This is useful to crop a grid created on #' the basis of a different raster coordinates on top of a different raster, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'   # create a fishnet over an input raster file and save it as a shapefile
#'   library(raster)
#'   library(sprawl)
#'   library(rasterVis)
#'
#'   in_rast  <- build_testraster(20,20,1)
#'
#'   fishnet  <- create_fishnet(in_rast)
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   fishnet  <- create_fishnet(in_rast, pix_for_cell = c(2,2))
#'   plot_rast(in_rast, in_poly = fishnet)
#'
#'   fishnet  <- create_fishnet(in_rast, cellsize = c(22,50))
#'   plot_rast(in_rast, in_poly = fishnet)
#'  }
#' @seealso
#'  \code{\link[raster]{extent}},\code{\link[raster]{res}}
#'  \code{\link[sf]{st_as_sfc}},\code{\link[sf]{st_make_grid}}
#' @rdname create_fishnet
#' @export
#' @importFrom raster extent res
#' @importFrom sf st_as_sfc st_make_grid st_sf
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
create_fishnet <- function(in_rast,
                           pix_for_cell = 1,
                           cellsize     = NULL,
                           to_file      = FALSE,
                           out_shape    = NULL,
                           overwrite    = TRUE,
                           crop_layer   = NULL) {

  #TODO
  # modify automatically the extent so that the cellsize is ALWAYS respected, even if it
  # is not a submultiple of the extent.

  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####
  #

  in_type <- check_spatype(in_rast)
  if (!(in_type %in% c("rastobject", "rastfile"))) {
    stop("create_fishnet --> ", in_rast, "is not an *Raster object or raster file. Aborting !")
  }

  if (in_type == "rastfile") {
    in_rast <- raster::raster(in_rast)
  }

  bbox     <- raster::extent(in_rast)

  if (is.null(cellsize)) {
    cellsize <- raster::res(in_rast) * pix_for_cell
  } else {
    if (length(cellsize) == 1) {
      cellsize = c(cellsize, cellsize)
    }
  }

  ext_poly <- sf::st_as_sfc(c(paste0("POLYGON((",
                                     bbox[1], " ", bbox[3], ", ",
                                     bbox[1], " ", bbox[4], ", ",
                                     bbox[2], " ", bbox[4], ", ",
                                     bbox[2], " ", bbox[3], ", ",
                                     bbox[1], " ", bbox[3], "",
                                     "))")),
                            crs = sp::proj4string(in_rast))

  geometry <- sf::st_make_grid(ext_poly,
                               cellsize,
                               what = "polygons")
  fish <- sf::st_sf(cell_id = seq_len(length(geometry)[1]),
                    geometry = geometry)
  if (!to_file) {
    return(fish)
  } else {
    if (is.null(out_shape)) {
      out_shape <- tempfile(fileext = ".shp")
    }
    write_shape(fish, out_shape, overwrite = TRUE)
  }
}
