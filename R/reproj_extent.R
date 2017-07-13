#' @title extent_reproj
#' @description accessory function to convert the extent of a spatial object to a different
#' projection. simple wrapper for the `spTransform` function on an [`raster::extent`] object
#'
#' @param ext_in   object of class [`raster::extent`]
#' @param in_proj  `character` proj4string representing the projection of the input extent
#' @param out_proj `character` proj4string representing the desired projection for the output extent
#'
#' @return reprojected extent (in out_proj projection)
#' @export
#'
#' @importFrom raster extent
#' @importFrom sf st_multipoint st_geometry st_set_crs st_transform st_bbox
#' @importFrom sp SpatialPoints CRS spTransform
#' @importFrom magrittr %>%
#'
#' @examples
#'

reproj_extent <- function(ext_in, in_proj = NULL, out_proj = NULL) {

  if (is.null(in_proj) | is.null(out_proj)) {
    stop("Input or Output projection not set ! Aborting !")
  }

  if (class(try(sp::CRS(in_proj), silent = TRUE)) == "try-error") {
    stop("reproj_extent --> Invalid input projection ! Aborting !")
  }

  if (class(try(sp::CRS(out_proj), silent = TRUE)) == "try-error") {
    # print(paste("MY_ERROR:  ",err))
    stop("reproj_extent --> Invalid input projection ! Aborting !")
  }

  # If in_proj and out_proj differ, reproject the shape extent

  if (in_proj != out_proj) {
    if (inherits(ext_in, "Extent")) {

      out_ext_rep <- data.frame(x = c(ext_in@xmin, ext_in@xmax),
                                y = c(ext_in@ymax, ext_in@ymin))          %>%
        sp::SpatialPoints(proj4string = sp::CRS(in_proj)) %>%
        sp::spTransform(out_proj) %>%
        raster::extent()

    } else {
      if (inherits(ext_in, "bbox")) {
        out_ext_rep <- sf::st_multipoint(rbind(ext_in[1:2], ext_in[3:4])) %>%
          sf::st_geometry() %>%
          sf::st_set_crs(in_proj) %>%
          sf::st_transform(out_proj) %>%
          sf::st_bbox()
      } else {
        stop("Input is not a `raster` extent or `sf` bbox. Aborting !")
      }
    }
  } else {
    message("Input and output projection are identical ! Doing Nothing ! ")
    out_ext_rep <- ext_in
  }
  return(out_ext_rep)
}
