#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_vect PARAM_DESCRIPTION
#' @param min_patch_area PARAM_DESCRIPTION
#' @param reassign_met PARAM_DESCRIPTION
#' @param out_type PARAM_DESCRIPTION, Default: NULL
#' @param out_file PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: NULL
#' @param create_dir PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sieve_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom sf st_geometry st_area st_polygon summarise.sf
#' @export summarise.sf


sieve_vect <- function(in_vect,
                       min_patch_area,
                       class_field,
                       reassign_met = "bord_lgt",
                       out_type = "sfobject",
                       out_file = NULL,
                       overwrite = FALSE,
                       create_dir = FALSE,
                       verbose = TRUE) {

  names(in_vect)[names(in_vect) == class_field] = "idsieve"

    # browser()
  areas         <- as.numeric(sf::st_area(in_vect))

  in_vect_big   <- in_vect[which(areas >= min_patch_area), ]
  in_geom <- sf::st_geometry(in_vect_big)
  for (pol in seq_along(in_geom)){
    curr_poly  <- in_geom[pol]
    pol_filled <- sf::st_polygon(curr_poly[[1]][1])
    if (length(curr_poly[[1]]) > 1) {
      h_ind <- 2
      for (hole in 2:length(curr_poly[[1]])) {
        hole_area <- sf::st_polygon(curr_poly[[1]][hole]) %>% sf::st_area()
        if (hole_area >= min_patch_area) {
          pol_filled[h_ind] <- sf::st_polygon(curr_poly[[1]][hole])
          h_ind <- h_ind + 1
        }
      }
    }
    in_geom[pol] <- pol_filled
  }
  in_vect_filled <- sf::st_set_geometry(in_vect_big, sf::st_geometry(in_geom))


  # fill "holes within holes"
  in_vect_small <- in_vect[which(areas < min_patch_area), ]
  in_geom <- sf::st_geometry(in_vect_small)
  for (pol in seq_along(in_geom)){
    curr_poly  <- in_geom[pol]
    pol_filled <- sf::st_polygon(curr_poly[[1]][1])
    if (length(curr_poly[[1]]) > 1) {
      h_ind <- 2
      for (hole in 2:length(curr_poly[[1]])) {
        hole_area <- sf::st_polygon(curr_poly[[1]][hole]) %>% sf::st_area()
        if (hole_area >= min_patch_area) {
          pol_filled[h_ind] <- sf::st_polygon(curr_poly[[1]][hole])
          h_ind <- h_ind + 1
        }
      }
    }
    in_geom[pol] <- pol_filled
  }
  in_vect_small_filled <- sf::st_set_geometry(in_vect_small, sf::st_geometry(in_geom))

  # fill small polygons that were not holes based on shared boundaries
  # with other "classes"
  relates        <- sf::st_relate(in_vect_small_filled, in_vect_filled)
  which_nothole  <- apply(
    relates,
    FUN = function(x) !any(stringr::str_sub(x, 1, 1) == 2),
    MARGIN = 1)
  notholes       <- in_vect_small_filled[which_nothole,]

  if (reassign_met == "bord_lgt") {
    notholes$id       <- row.names(notholes)
    in_vect_filled$id <- row.names(in_vect_filled)
    bord_lines     <- sf::st_intersection(notholes, in_vect_filled) %>%
      dplyr::mutate(lgt = sf::st_length(.)) %>%
      dplyr::filter(as.numeric(lgt) != 0) %>%
      dplyr::arrange(id, lgt) %>%
      dplyr::group_by(id) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      sf::st_as_sf()

    notholes <- notholes %>%
      dplyr::arrange(!!col_name) %>%
      dplyr::mutate(idsieve = bord_lines[[paste0("idsieve", ".1")]]) %>%
      sf::st_as_sf()

  }

  # re-add notholes to in_vect_filled, with reassigned class, and dissolve
  # boundaries
  in_vect_filled <- rbind(in_vect_filled, notholes) %>%
    group_by(idsieve) %>%
    sf::st_as_sf() %>%
    summarise(do_union = TRUE)

  names(in_vect_filled)[names(in_vect_filled) == "idsieve"] = class_field


  if (out_type == "sfobject") {
    return(in_vect_filled)
  }

  if (out_type == "vectfile") {
    if (is.null(out_file)) {
      out_file <- tempfile(fileext = ".shp")
    }

    sprawl::write_shape(in_vect_filled,
                        out_file,
                        overwrite  = overwrite,
                        verbose    = FALSE,
                        create_dir = create_dir)
    return(out_file)
  }
}
