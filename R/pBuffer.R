#' pBuffer
#' @description Create a \code{\linkS4class{SpatialPolygons}*} object expanding a
#' \code{\linkS4class{SpatialPoints}*} with single buffers. The new geometries are rectangles
#' or circles which maintain the original associated \code{data.frame}, if present.
#' The width/weight or radius of the buffers are directly provided to the function of taken from a sample
#' \code{\linkS4class{Raster}*} / \code{\linkS4class{SpatialGrid}*} / \code{\linkS4class{SpatialPixels}*}
#' object (see details).
#'
#' @param points A \code{\linkS4class{SpatialPoints}} or \code{\linkS4class{SpatialPointsDataFrame}}
#' object, including the points to be buffered.
#' @param res A numeric vector of length 1 or 2, containing the radius of the circular buffers (if
#' 1-length vector) or the width and height of the rectangle (if 2-length).
#' Alternatively, also a \code{\linkS4class{Raster}*} / \code{\linkS4class{SpatialGrid}*} /
#' \code{\linkS4class{SpatialPixels}*} is accepted: in this case, the raster resolution is used
#' as width/height of the rectangular buffers.
#' @param res_scale (optional) Numeric scaling factor of the resolution (useful in the case \code{res}
#' is a raster object).
#' @param quadsegs (optional) Number of line segments to use to approximate a quarter circle
#' (in case of circular buffer).
#' @param ... other arguments
#'
#' @return An object of class \code{\linkS4class{SpatialPolygons}} or
#' \code{\linkS4class{SpatialPolygonsDataFrame}},
#' depending on the specific geometry which is read.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#'
#' @importFrom raster res
#' @importFrom rgeos gBuffer
#' @importFrom sp coordinates SpatialPoints SpatialPolygons Polygons Polygon
#' @importFrom methods is as

#' @examples \dontrun{
#' # Load sample data
#' r <- raster::raster(system.file("extdata","testrast.tif", package = "sprawl"))
#' p <- sp::spsample(as(r, "SpatialPixels"), 20, "random")
#'
#' # Rectangular buffer (3x3 cells)
#' pBuffer(p, r, 3)
#'
#' # Squared buffer (100x100 metres)
#' pBuffer(p, c(100,100))
#'
#' # Circular buffer (radius = 50 metres)
#' pBuffer(p, 50)
#' }
#'

pBuffer <- function(points, res, res_scale=1, quadsegs=5, ...) {

  # Check res data type
  if (methods::is(res,"SpatialGrid") | methods::is(res,"SpatialGrid")) {
    raster_res <- res@grid@cellsize
    style <- "rectangle"
  } else if (methods::is(res,"Raster")) {
    raster_res <- raster::res(res)
    style <- "rectangle"
  } else if (methods::is(res,"numeric")) {
    raster_res <- res
    if (length(res) == 1) {
      style <- "circle"
    } else if (length(res) == 2) {
      style <- "rectangle"
    } else  {
      stop("Please provide a vector of length 1 or 2.")
    }
  } else stop("res is not recognised; please provide it as numeric vector or raster object.")


  # Compute buffers
  buffer_list <- list()
  for (i in 1:length(points)) {

    if (style == "rectangle") {
      buffer_coords <- t(matrix(c(sp::coordinates(points[i,])[1:2] + raster_res*res_scale*c(.5,.5),
                                  sp::coordinates(points[i,])[1:2] + raster_res*res_scale*c(.5,-.5),
                                  sp::coordinates(points[i,])[1:2] + raster_res*res_scale*c(-.5,-.5),
                                  sp::coordinates(points[i,])[1:2] + raster_res*res_scale*c(-.5,.5)),
                         nrow = 2))
      buffer_points    <- sp::SpatialPoints(buffer_coords, proj4string = points@proj4string)
      buffer_list[[i]] <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(buffer_points, hole = TRUE))
                                                                , i)),proj4string = points@proj4string)
    } else if (style == "circle") {
      buffer_list[[i]] <- rgeos::gBuffer(points[i,], width = raster_res*res_scale, quadsegs = quadsegs, id = i)
    }

  }

  buffers <- sp::SpatialPolygons(lapply(buffer_list, function(x){x@polygons[[1]]}))


  # Join data.frame, if exists
  if (methods::is(points,"SpatialPointsDataFrame")) {
    buffers      <- methods::as(buffers,"SpatialPolygonsDataFrame")
    buffers@data <- points@data
  }

  return(buffers)

}
