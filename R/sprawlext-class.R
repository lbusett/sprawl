#' @title sprawlext class
#' @name sprawlext
#' @rdname sprawlext
#' @description An S4 class to represent the extent of a spatial object,
#'  associated with its proj4string.
#' @slot extent `numeric (4)` extent of the object (xmin, ymin, xmax, ymax)
#' @slot proj4string `character` proj4string of the object
#' @exportClass sprawlext
#' @importFrom methods setClass representation
#' @importFrom sp CRS
#' @examples
#' showClass("sprawlext")
#'
#' \dontrun{
#'
#' library(raster)
#'
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'   package = "sprawl.data")
#' ex_sprawlext <- get_extent(in_rast)
#'
#' # Convert to Extent
#' ex_extent <- as(ex_sprawlext, "Extent")
#' ex_extent
#'
#' # Convert to bbox
#' ex_bbox <- as(ex_sprawlext, "matrix")
#' ex_bbox
#'
#' # Convert to Spatial* objects
#' as(ex_sprawlext, "SpatialPolygons")
#' as(ex_sprawlext, "SpatialPoints")
#' as(ex_sprawlext, "SpatialLines")
#'
#' # Extract proj4string CRS
#' ex_crs <- as(ex_sprawlext, "CRS")
#' ex_crs
#'
#' # Create sprawlext from extent and CRS
#' get_extent(ex_extent, ex_crs)
#'
#' # Create sprawlext from bbox and CRS
#' get_extent(ex_bbox, ex_crs)
#' }

methods::setClass("sprawlext",
                  methods::representation(extent = "numeric",
                                          proj4string = "character"))

setAs("sprawlext", "Extent", function(from) {
  return(extent(from@extent[c(1,3,2,4)]))
})

setAs("sprawlext", "matrix", function(from) {
  out_bbox <- matrix(from@extent, nrow=2, ncol=2)
  dimnames(out_bbox) <- list(c("x","y"), c("min","max"))
  return(out_bbox)
})

setAs("sprawlext", "CRS", function(from) {
  return(CRS(from@proj4string))
})

setAs("sprawlext", "SpatialPolygons", function(from) {
  out_poly <- as(extent(from@extent[c(1,3,2,4)]), "SpatialPolygons")
  out_poly@proj4string <- CRS(from@proj4string)
  return(out_poly)
})

setAs("sprawlext", "SpatialLines", function(from) {
  out_poly <- as(extent(from@extent[c(1,3,2,4)]), "SpatialLines")
  out_poly@proj4string <- CRS(from@proj4string)
  return(out_poly)
})

setAs("sprawlext", "SpatialPoints", function(from) {
  out_poly <- as(extent(from@extent[c(1,3,2,4)]), "SpatialPoints")
  out_poly@proj4string <- CRS(from@proj4string)
  return(out_poly)
})
