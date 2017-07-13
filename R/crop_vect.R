#' @title crop_vect
#' @description Function allowing to crop an input vector object or shapefile on the extent of a
#' different spatial object. in_vect must be an *sp, *sf object or vector file, while in_obj should
#' be a valid spatial object (sp, sf, vector file, raster or rasterfile)
#' @param in_vect PARAM_DESCRIPTION
#' @param in_obj PARAM_DESCRIPTION
#' @param to_file PARAM_DESCRIPTION, Default: FALSE
#' @param as_sp PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sp]{proj4string}}
#' @rdname crop_vect
#' @export
#' @importFrom raster extent raster crop
#' @importFrom sf st_as_sf st_bbox st_crs st_as_sfc st_set_agr st_transform st_intersection
#' @importFrom sp proj4string
#'
crop_vect <- function(in_vect,
                      in_obj,
                      to_file = FALSE,
                      as_sp   = FALSE) {

  #   ____________________________________________________________________________
  #   Check arguments                                                         ####
  vectype <- check_spatype(in_vect)
  objtype <- check_spatype(in_obj)

  if (!vectype %in% c("sfobject", "spobject", "vectfile")) {
    stop("crop_sf --> in_vect is not a valid vector object (sp, sf or vector file")
  }

  if (objtype == "none") {
    stop("crop_sf --> in_obj is not a valid spatial object (sp, sf,
                                           vector file, raster or rasterfile")
  }

  #   ____________________________________________________________________________
  #   If both in_vect and in_obj are sp objects, use `raster::crop`           ####

  if (vectype == "spobject" & objtype == "spobject") {
    out_vec  <- crop(in_vect, in_obj)
    if (!as_sp) {
      out_vec <- sf::st_as_sf(out_vec)
    }
    return(out_vec)
  }

  #   ____________________________________________________________________________
  #   Otherwise,  retrieve the extent of in_obj, then perform cropping       ####
  #   using st_intersection or crop on the basis of the class of in_vect

  if (objtype == "none") {
    stop("crop_sf --> in_obj is not a valid spatial object (sp, sf,
                                           vector file, raster or rasterfile")
  }
  if (objtype %in% c("spobject", "rastobject")) {
    ext_in  <- raster::extent(in_obj)
    crs_ext <- proj4string(in_obj)
  }
  if (objtype == "rastfile") {
    ext_in <- in_obj %>%
      raster::raster() %>%
      raster::extent()
    crs_ext <- proj4string(in_obj)
  }
  if (objtype == "sfobject"){
    ext_in  <- sf::st_bbox(in_obj)[c(1,3,2,4)]
    crs_ext <- st_crs(ext_in)
  }
  if (objtype == "vectfile") {
    ext_in <- in_obj %>%
      readshape() %>%
      raster::extent()[c(1,3,2,4)]
    crs_ext <- st_crs(ext_in)
  }

  if (vectype == "vectfile") {
    in_vect  <- readshape(in_vect)
  }

  if (vectype %in% c("vectfile", "sfobject")) {
    crs_vect <- sf::st_crs(in_vect)$proj4string
  } else {
    crs_vect <- sp::proj4string(in_vect)
  }

  if (vectype == "sfobject") {
    ext_poly   <- sf::st_as_sfc(c(paste0("POLYGON((",
                                         ext_in[1], " ", ext_in[3], ", ",
                                         ext_in[1], " ", ext_in[4], ", ",
                                         ext_in[2], " ", ext_in[4], ", ",
                                         ext_in[2], " ", ext_in[3], ", ",
                                         ext_in[1], " ", ext_in[3], "",
                                         "))")),
                                crs = crs_ext)

    in_vect  <- sf::st_set_agr(in_vect, "constant")
    # ext_poly <- sf::st_set_agr(ext_poly,  "constant")
    if (!(crs_ext == crs_vect)) {
      ext_poly <- sf::st_transform(ext_poly, crs_vect)
    }
    crop_vect <- sf::st_intersection(in_vect, ext_poly)

    if (as_sp) {
      crop_vect <- as(crop_vect, "Spatial")
    }
  } else {
    if (!(crs_ext == crs_vect)) {
      ext_poly <- sf::st_transform(ext_poly, crs_vect)
    }
    crop_vect <- raster::crop(in_vect, ext_in)
    if (!as_sp) {
      crop_vect <- sf::st_as_sf(crop_vect)
    }
  }
  return(crop_vect)
}
