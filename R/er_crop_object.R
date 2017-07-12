#' @title er_crop_object
#' @description Accessory function for `extract_rast`. Crops the in_vect_zones on the extent of the raster
#' to improve speed. Then check if some features in the original in_vect_zones were dropped in
#' zone_raster_croppped
#' @param in_vect_zones PARAM_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param id_field PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION
#' @return `list` with the following fields:
#' - in_vect_zones_crop = cropped `sf` object
#' - outside_feat = data frame containg info on the "removed" features (sequential id and corresponding
#' value ofthe "id" field (if provided)

#' @importFrom raster extent
#' @importFrom sf st_intersection st_set_crs st_as_sf st_crs st_bbox
#' @importFrom tibble as_data_frame
er_crop_object <- function(in_vect_zones,
                           in_rast,
                           id_field,
                           verbose) {

  #   ____________________________________________________________________________
  #   Crop input `sf` object to the extent of the input raster                ####

  in_vect_zones_crop <- crop_vect(in_vect_zones, in_rast)


  # ext_sf <- sf::st_set_crs(sf::st_as_sf(as(raster::extent(in_rast[[1]]),
  #                                          "SpatialPolygons")),
  #                          sf::st_crs(in_vect_zones))
  #
  # in_vect_zones_crop <- sf::st_intersection(in_vect_zones, st_geometry(ext_sf))
  #
  # in_vect_zones_crop <- sf::st_intersection(st_geometry(in_vect_zones),
  #                                           st_geometry(ext_sf))

  #   ____________________________________________________________________________
  #   Check if the cropped file has different extension (i.e., it was croppped) ####
  #   If so, check if any features were dropped because outside the raster extent
  #   and save the "ids" of those features in `outside_feat`

  if (!isTRUE(all.equal(sf::st_bbox(in_vect_zones_crop),
                        (sf::st_bbox(in_vect_zones)), scale = 100))) {
    if(verbose) {
      message("Some features of the spatial object are outside or partially outside the extent of the input RasterStack !
  Outputs for features only partially inside will be retrieved using only the available pixels !
  Outputs for features outside rasterstack extent will be set to NA."
      )
    }
    if (!setequal(in_vect_zones$mdxtnq, in_vect_zones_crop$mdxtnq)) {

      outside_ids   <- setdiff(in_vect_zones$mdxtnq, in_vect_zones_crop$mdxtnq)
      outside_names <- ifelse(!is.null(id_field),
                              as.character(tibble::as_data_frame(in_vect_zones[outside_ids,eval(id_field)])[,1]),
                              rep(NA, length(outside_ids)))
      outside_feat  <- data.frame(outside_ids   = in_vect_zones$mdxtnq[outside_ids],
                                  outside_names = outside_names)

    } else {
      outside_feat  <- NULL
    }
  } else {
    outside_feat  <- NULL
  }
  return(list(in_vect_zones_crop = in_vect_zones_crop, outside_feat = outside_feat))
}
