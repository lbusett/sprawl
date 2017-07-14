#' @title mask a raster file based on a vector file
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param mask_shape PARAM_DESCRIPTION
#' @param out_nodata PARAM_DESCRIPTION, Default: 'NA'
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

#'  \code{\link[gdalUtils]{gdalsrsinfo}}

#'  \code{\link[raster]{raster}},\code{\link[raster]{extent}}
#' @rdname mask_rast
#' @export
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom raster raster extent
#' @importFrom sf st_buffer st_crs st_transform st_as_sf st_combine st_sf
#'
mask_rast <- function(in_rast, mask_shape, out_nodata = "NA", buffer = NULL, verbose = TRUE) {
  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####

  # checks on in_rast
  ras_type <- check_spatype(in_rast)
  if (check_spatype(in_rast) == "rastfile") {
    in_rast  <- raster::raster(in_rast)
    ras_type <- "rastobject"
  }
  if (ras_type != "rastobject") {
    stop("mask_rast --> `in_rast` must be a `*Raster` object or raster file name. Aborting !")
  }
  rast_proj <- proj4string(in_rast)

  # checks on mask_shape
  shp_type <- check_spatype(mask_shape)

  if (shp_type == "none") {
    stop("mask_rast --> ", basename(mask_shape), "is not a vector file or object. Aborting !" )
  }

  temp_shapefile <- tempfile(fileext = ".shp")
  if (shp_type == "vectfile") {
    if (!is.null(buffer)) {
      mask_shape <- read_shape(mask_shape) %>%
        sf::st_buffer(., buffer)
      mask_proj <- sf::st_crs(mask_shape)$proj4string
      if (mask_proj != rast_proj) {
        mask_shape <- sf::st_transform(temp_shape, rast_proj)
      }
      write_shape(mask_shape, temp_shapefile, overwrite = TRUE)
    } else {
      mask_proj <- gdalUtils::gdalsrsinfo(mask_shape, as.CRS = T)@projargs
      if (mask_proj != rast_proj) {
        mask_shape <- read_shape(mask_shape) %>%
          sf::st_transform(rast_proj) %>%
          write_shape(temp_shapefile)
      } else {
        temp_shapefile <- mask_shape
      }
    }
  } else {
    if (shp_type == "spobject") {
      mask_shape <- sf::st_as_sf(mask_shape)
    }
    mask_shape <- mask_shape %>%
      sf::st_combine() %>%
      sf::st_sf(id = 1, .)
    if (!is.null(buffer)) {
      mask_shape <- read_shape(mask_shape) %>%
        sf::st_buffer(buffer)
    }
    mask_proj <- sf::st_crs(mask_shape)$proj4string
    if (mask_proj != rast_proj) {
      mask_shape <- sf::st_transform(mask_shape, rast_proj)
    }
    write_shape(mask_shape, temp_shapefile, overwrite = TRUE)
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


  #   ____________________________________________________________________________
  #   correct the extent for gdal_rasterize to get                            ####
  #   an identical sized output
  te = raster::extent(in_rast)[c(1, 3, 2, 4)][] - c(0, -res(in_rast)[1], res(in_rast)[1], 0)

  #   ____________________________________________________________________________
  #   Rasterize the msk shapefile                                             ####

  temp_rasterfile = tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-a_nodata", out_nodata,
                            "-te", paste(te, collapse = " "),
                            "-tr", paste(res(in_rast), collapse = " "),
                            "-ot", ot,
                            "-tap",
                            temp_shapefile,
                            temp_rasterfile)

  system2(normalizePath(file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "gdal_rasterize")),
          args = rasterize_string,
          stdout = NULL)
  tempmask <- raster(temp_rasterfile)
  # browser()
  masked <- in_rast * tempmask
  file.remove(temp_shapefile)
  return(masked)

}
