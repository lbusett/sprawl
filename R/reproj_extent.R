#' @title reproject the extent of a spatial object
#' @description accessory function to convert the extent of a spatial object to
#'  a different projection. simple wrapper for the `spTransform` function on
#' @param ext  object of class `sprawlext`, or any other object or
#'  filename from which a `sprawlext` object can be derived (see `get_extent`)
#' @param out_proj `character` proj4string representing the desired projection for the output extent
#' @param in_proj  `character` (optional) proj4string representing the projection of
#'  projection (like [`extent`] or [`bbox`]).
#'  the input extent. It is needed only if ext is an object which does not include a
#' @param enlarge `logical` If TRUE (default), the reprojected bounding box is the
#' one which completely include the original one; if FALSE (faster), it is simply
#' the one obtained by reprojecting the upper-left and the lower-right corners.
#' @param n_dens `numeric` Densification ratio used in the case enlarge is TRUE.
#'  reprojected extent
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @name reproj_extent
#' @rdname reproj_extent
#' @return An object of class `sprawlext` representing the reprojected extent
#' @export
#' @importFrom rgdal CRSargs
#' @importFrom sf st_polygon st_sfc st_transform
#' @importFrom sp CRS
#' @importFrom magrittr %>%
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @examples
#' \dontrun{
#' library(raster)
#' in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'   package = "sprawl.data")
#' in_rast <- raster::raster(in_file)
#' in_sprawlext <- get_extent(in_rast)
#'
#' # Reproject a sprawlext
#' reproj_extent(in_sprawlext, "+init=epsg:32651")
#'
#' # Reproject the extent of a file
#' reproj_extent(in_file, "+init=epsg:32651")
#'
#' # Reproject the extent of a raster
#' reproj_extent(in_rast, "+init=epsg:32651")
#'
#' # Reproject an extent or a bounding box (given the input projection)
#' reproj_extent(raster::extent(in_rast), "+init=epsg:32651", in_proj=in_rast@crs)
#' reproj_extent(sp::bbox(in_rast), "+init=epsg:32651", in_proj=in_rast@crs)
#' # (this fails without providing in_proj value:)
#' reproj_extent(sp::bbox(in_rast), "+init=epsg:32651")
#'
#' # Reproject without enlarging
#' reproj_extent(in_sprawlext, "+init=epsg:32651", enlarge = FALSE)
#' }


reproj_extent <- function(ext,
                          out_proj,
                          in_proj = NULL,
                          enlarge=TRUE,
                          n_dens=1E3,
                          verbose = TRUE) {

  if (verbose) {
    call <- match.call()
    message("crop_rast --> Reprojecting extent of: ", call[[2]],
            " to: ", call[[3]])
  }
  # Checks on projections
  out_proj <- check_proj4string(out_proj, abort = TRUE)
  if (!is.null(in_proj)) {
    in_proj <- check_proj4string(in_proj, abort = TRUE)
  }

  # Convert ext in sprawlext
  ext <- get_extent(ext, proj4string = in_proj, abort = TRUE)

  # If in_proj and out_proj differ, reproject the shape extent

  if (rgdal::CRSargs(sp::CRS(out_proj)) != rgdal::CRSargs(sp::CRS(ext@proj4string))) { #nolint

    if (enlarge) {
      in_ext <- data.frame(
        lon = c(ext@extent["xmin"] +
                  diff(ext@extent[c("xmin","xmax")]) * (0:n_dens) / n_dens,
                rep(ext@extent["xmax"], n_dens - 1),
                ext@extent["xmin"] +
                  diff(ext@extent[c("xmin","xmax")]) * (n_dens:0) / n_dens,
                rep(ext@extent["xmin"], n_dens - 1),
                ext@extent["xmin"]),
        lat = c(rep(ext@extent["ymin"], n_dens),
                ext@extent["ymin"] +
                  diff(ext@extent[c("ymin","ymax")]) * (0:n_dens) / n_dens,
                rep(ext@extent["ymax"], n_dens - 1),
                ext@extent["ymin"] +
                  diff(ext@extent[c("ymin","ymax")]) * (n_dens:1) / n_dens,
                ext@extent["ymin"])) %>%
        as.matrix() %>%
        list() %>%
        sf::st_polygon() %>%
        sf::st_sfc(crs = ext@proj4string)
    } else {
      in_ext <- as(ext, "sfc_POLYGON") # convert in a sfc_POINT
    }

    out_ext_rep <- sf::st_transform(in_ext, out_proj) %>%
      get_extent()

  } else {
    if (verbose) message("Input and output projection are identical! ",
                         "Doing Nothing!")
    out_ext_rep <- ext
  }
  return(out_ext_rep)

}
