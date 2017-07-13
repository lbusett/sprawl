#' @title mask_rast
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param mask_shape PARAM_DESCRIPTION
#' @param buffer PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' library(raster)
#' in_polys <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl"), stringsAsFactors = T)
#' in_rast  <- raster::stack(system.file("extdata", "testrast.tif", package = "sprawl"))[[1]]
#' in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
#' masked   <- mask_rast(in_rast, in_polys, verbose = FALSE)
#' par(mfrow=c(1,2))
#' plot(in_rast)
#' plot(masked)
#' }
#' @seealso
#'  \code{\link[dplyr]{case_when}}

#'  \code{\link[gdalUtils]{gdal_rasterize}}

#'  \code{\link[raster]{raster}},\code{\link[raster]{origin}}
#' @rdname mask_rast
#' @export
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom raster raster origin
#' @importFrom sf st_combine st_cast st_buffer st_sf
#'
mask_rast <- function(in_rast, mask_shape, buffer = NULL, verbose = TRUE) {
  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####

  # TODO : automatic reprojection of the clipper !!!

  # checks on in_rast
  ras_type <- check_spatype(in_rast)
  if (check_spatype(in_rast) == "rastfile") {
    in_rast  <- raster::raster(in_rast)
    ras_type <- "rastobject"
  }
  if (ras_type != "rastobject") {
    stop("mask_rast --> `in_rast` must be a `*Raster` object or raster file name. Aborting !")
  }

  # checks on mask_shape
  shp_type <- check_spatype(mask_shape)

  if (shp_type == "vectfile") {
    temp_shapefile <- mask_shape
    mask_shape     <- read_shape(mask_shape) %>%
      sf::st_combine() %>%
      sf::st_cast("MULTIPOLYGON")
    if (!is.null(buffer)) {
      if (verbose) message("mskrast -> Buffering the mask extent")
      mask_shape <- mask_shape %>%
        sf::st_buffer(buffer)
      write_shape(mask_shape, temp_shapefile, overwrite = TRUE)
    }
  }

  if (shp_type %in% c("sfobject", "spobject")) {
    if (shp_type == "spobject") mask_shape <- st_as_sf(mask_shape)
    mask_shape     <- sf::st_combine(mask_shape) %>%
      sf::st_sf(id = 1, .)
    if (!is.null(buffer)) {
      if (verbose) message("mskrast -> Buffering the mask extent")
      mask_shape <- mask_shape %>%
        sf::st_buffer(buffer)
    }
    temp_shapefile <- tempfile(tmpdir = tempdir(), fileext = ".shp")
    shp_type       <- "vectfile"
    write_shape(mask_shape, temp_shapefile, overwrite = TRUE)
  }

  if (shp_type != "vectfile") {
    stop("mask_rast --> `in_obj` must be a `sp` or `sf` object or valid shapefile file name.
         Aborting !")
  }

  #   ____________________________________________________________________________
  #   rasterize mask_shape                                                    ####

  if (verbose) {(message("mask_rast --> Writing temporary rasterized shapefile"))}
  temp_rasterfile <- tempfile(tmpdir = tempdir(), fileext = ".tiff")
  max_id <- length(mask_shape)
  ot <- dplyr::case_when(
    (max_id <= 255) == 1 ~ "Byte",
    (max_id >= 255 & max_id < 65535) == 1 ~ "Int16",
    (max_id >= 65536) == 1 ~ "Int32"
  )
  te = extent(in_rast)[c(1,3,2,4)][] - c(0,0,res(in_rast)[1],0)
  temp_rasterfile = tempfile(tmpdir = tempdir(), fileext = ".tif")
  rastzone_object <- gdalUtils::gdal_rasterize(temp_shapefile,
                                               temp_rasterfile,
                                               tr = res(in_rast),
                                               te = te,
                                               burn = 1,
                                               at = FALSE,
                                               ot = ot,
                                               tap = F,
                                               output_Raster = TRUE,
                                               a_nodata = NA,
                                               verbose = T
  )

  raster::origin(rastzone_object) = raster::origin(in_rast)
  masked <- in_rast*rastzone_object

  return(masked)

}
