#' @title reproject the extent of a spatial object
#' @description accessory function to convert the extent of a spatial object to a different
#' projection. simple wrapper for the `spTransform` function on an [`raster::extent`] object
#'
#' @param ext   object of class [`sprawlext`][sprawl::get_extent()]
#' @param out_proj `character` proj4string representing the desired projection for the output extent
#' @param enlarge `logical` If TRUE, the reprojected bounding box is the
#' one which completely include the original one; if FALSE (defalt, faster), it is simply
#' the one obtained by reprojecting the upper-left and the lower-right corners.
#' @param n_dens `numeric` Densification ratio used in the case enlarge is TRUE.
#' @return An object of class [`sprawlext`][sprawl::get_extent()] representing the
#' reprojected extent
#' @export
#'
#' @importFrom raster extent
#' @importFrom rgdal CRSargs
#' @importFrom sf st_multipoint st_geometry st_set_crs st_transform st_bbox
#' @importFrom sp SpatialPoints CRS spTransform
#' @importFrom magrittr %>%
#'
#' @examples
#'

reproj_extent <- function(ext, out_proj = NULL, enlarge=TRUE, n_dens=1E3) {
  UseMethod("reproj_extent")
}


#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname reproj_extent
#' @method reproj_extent default
#' @export
reproj_extent.default  <- function(ext, out_proj = NULL, enlarge=TRUE, n_dens=1E3) {
  stop("reproj_extent --> ", deparse(substitute(ext)), " is not a valid extent of class `sprawlext`;",
       " see ?get_extent for details.")
}


#   ____________________________________________________________________________
#   Method for "sprawlext"                                                  ####

#' @rdname reproj_extent
#' @method reproj_extent sprawlext
#' @export
reproj_extent.sprawlext <- function(ext, out_proj = NULL, enlarge=TRUE, n_dens=1E3) {

  # Checks on out_proj
  if (is.null(out_proj)) {
    stop("Output projection not set! Aborting!")
  }
  if (class(try(sp::CRS(out_proj), silent = TRUE)) == "try-error") {
    # print(paste("MY_ERROR:  ",err))
    stop("reproj_extent --> Invalid input projection! Aborting!")
  }


  # If in_proj and out_proj differ, reproject the shape extent

  if (CRSargs(CRS(out_proj)) != CRSargs(CRS(ext@projstring))) {

    if (enlarge) {
      in_ext <- data.frame(
        lon = c(ext@extent["xmin"] +
                  diff(ext@extent[c("xmin","xmax")]) * (0:n_dens) / n_dens, rep(ext@extent["xmax"], n_dens - 1),
                ext@extent["xmin"] +
                  diff(ext@extent[c("xmin","xmax")]) * (n_dens:0) / n_dens, rep(ext@extent["xmin"], n_dens - 1)),
        lat = c(rep(ext@extent["ymin"], n_dens),
                ext@extent["ymin"] +
                  diff(ext@extent[c("ymin","ymax")]) * (0:n_dens) / n_dens, rep(ext@extent["ymax"], n_dens - 1),
                ext@extent["ymin"] +
                  diff(ext@extent[c("ymin","ymax")]) * (n_dens:1) / n_dens))
        in_ext <- list(Polygons(list(Polygon(in_ext)), 1)) %>%
          sp::SpatialPolygons(proj4string = sp::CRS(ext@projstring)) # convert in a SpatialPolygons
    } else {
      in_ext <- data.frame(
        x = ext@extent[rep(c("xmin","xmax"),times=2)],
        y = ext@extent[rep(c("ymin","ymax"),each=2)]) %>%
        sp::SpatialPoints(proj4string = sp::CRS(ext@projstring)) # convert in a SpatialPoints
    }

    out_ext_rep <- sp::spTransform(in_ext, out_proj) %>%
      get_extent()

  } else {
    message("Input and output projection are identical! Doing Nothing!")
    out_ext_rep <- ext
  }
  return(out_ext_rep)


}



# TODO:
# > migliorare classe sprawlext:
#   - definire metodi di conversione tra bbox, extent e sprawext
#   - creare help della classe


# proposed changes:
# > sprawlext@projstring -> sprawlext@proj4string (messing!)
# > sprawlext@projstring as CRS instead than character




