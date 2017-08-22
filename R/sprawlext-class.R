#' @title sprawlext class
#' @name sprawlext-class
#' @rdname sprawlext
#' @description An S4 class to represent the extent of a spatial object,
#'  associated with its proj4string.
#' @slot extent `numeric (4)` extent of the object (xmin, ymin, xmax, ymax)
#' @slot proj4string `character` proj4string of the object
#' @exportClass sprawlext
#' @importFrom methods setClass representation
#' @importFrom sp CRS
#' @importFrom sf st_polygon st_point
#' @importFrom magrittr %>%
#' @examples
#' showClass("sprawlext")
#'
#' \dontrun{
#' library(raster)
#'
#' in_rast <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'   package = "sprawl.data")
#' ex_sprawlext <- get_extent(in_rast)
#'
#' # Convert sprawlext to Extent
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
#' # Convert to sf objects
#' as(ex_sprawlext, "sfc_POLYGON")
#' as(ex_sprawlext, "sfc_POINT")
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

sprawlext <- methods::setClass(
  "sprawlext",
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

as.sprawlext.Spatial <- function(from, class) {
  out_sp <- as(extent(from@extent[c(1,3,2,4)]), class)
  out_sp@proj4string <- CRS(from@proj4string)
  return(out_sp)
}
setAs("sprawlext", "SpatialPoints", function(from) {
  as.sprawlext.Spatial(from, "SpatialPoints")
})
setAs("sprawlext", "SpatialLines", function(from) {
  as.sprawlext.Spatial(from, "SpatialLines")
})
setAs("sprawlext", "SpatialPolygons", function(from) {
  as.sprawlext.Spatial(from, "SpatialPolygons")
})

setAs("sprawlext", "sfc_POLYGON", function(from) {
  cbbox_ext <- from@extent
  out_st <- sf::st_polygon(list(rbind(
    c(cbbox_ext["xmin"], cbbox_ext["ymin"]),
    c(cbbox_ext["xmin"], cbbox_ext["ymax"]),
    c(cbbox_ext["xmax"], cbbox_ext["ymax"]),
    c(cbbox_ext["xmax"], cbbox_ext["ymin"]),
    c(cbbox_ext["xmin"], cbbox_ext["ymin"])))
  ) %>%
    sf::st_sfc(crs = from@proj4string)
  return(out_st)
})

setAs("sprawlext", "sfc_POINT", function(from) {
  cbbox_ext <- from@extent
  out_st <- sf::st_sfc(
    sf::st_point(c(cbbox_ext["xmin"], cbbox_ext["ymin"])),
    sf::st_point(c(cbbox_ext["xmin"], cbbox_ext["ymax"])),
    sf::st_point(c(cbbox_ext["xmax"], cbbox_ext["ymax"])),
    sf::st_point(c(cbbox_ext["xmax"], cbbox_ext["ymin"])),
    crs = from@proj4string)
  return(out_st)
})
