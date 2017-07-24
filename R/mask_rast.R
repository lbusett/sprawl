#' @title Mask a raster file (or object) based on a vector file (or object)
#' @description Masks a raster file or object on the basis of a vector file or object. Pixels not
#'   covered by the vector features are set to NoData. If the input raster is multi-band, the
#'   mask is automatically applied to all bands. An optional buffer can be applied to the input
#'   vector to allow a more "lenient" masking, or to remove also the borders of the vector.
#' @param in_rast Raster file or object inheriting class `raster` to be masked
#' @param mask_vect Vector file or object of class `*sf` or `sp` to be used as a mask
#' @param crop `logical` if TRUE, `in_rast` is also cropped on the extent of `mask_vect`,
#'   Default: FALSE
#' @param buffer `numeric` if not NULL, width of a buffer to be applied to `mask_vect` before
#'   masking `in_rast`. If negative, mask_vect is "reduced" prior to masking (see examples),
#'   Default: NULL
#' @param out_nodata `numeric` value to be assigned to areas outside the mask, Default: 'NoData'
#' @param to_file `logical` If TRUE, the output masked raster is save to file instead than sent back
#'   to the caller. In this case, the path to the saved file is returned instead
#' @param out_rast `character` filename where the masked raster should be saved (ignorted if
#'   to_file == FALSE). If NULL while to_file == TRUE, the masked raster is saved on a temporaty
#'   file in the `R` temporary folder. The file is saved in TIFF format, with `DAFLATE` compression.
#' @param out_dt TO BE CHECKED !!!!
#' @param verbose `logical` if TRUE, extended processing information is sent to the console in the
#'   form of messages
#' @return object of class `raster` (if to_file == FALSE), or `character` string corresponding to
#'   the filename of the created raster (if to_file == TRUE)
#' @details
#' @examples
#' \dontrun{
#' libray(sprawl)
#' libray(sprawl.data)
#' library(raster)
#' in_polys <- read_shape(system.file("extdata","lc_polys.shp", package = "sprawl.data"),
#'                        stringsAsFactors = T)
#' in_rast  <- raster::stack(system.file("extdata", "testrast.tif", package = "sprawl.data"))[[1]]
#' in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
#' masked   <- mask_rast(in_rast, in_polys, verbose = FALSE)
#' par(mfrow=c(1,2))
#' plot(in_rast)
#' plot(masked)
#' }
#' @rdname mask_rast
#' @export
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#' @importFrom dplyr case_when
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom raster raster extent writeRaster
#' @importFrom sf st_buffer st_crs st_transform st_as_sf st_combine st_sf

mask_rast <- function(in_rast,
                      mask_vect,
                      crop       = FALSE,
                      buffer     = NULL,
                      out_nodata = "NoData",
                      to_file    = FALSE,
                      out_rast   = NULL,
                      out_dt     = "FLT4S",
                      verbose    = TRUE) {

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
  rast_proj <- sp::proj4string(in_rast)

  # checks on mask_vect
  shp_type <- check_spatype(mask_vect)

  if (shp_type == "none") {
    stop("mask_rast --> ", basename(mask_vect), "is not a vector file or object. Aborting !" )
  }

  temp_shapefile <- tempfile(fileext = ".shp")

  #   ____________________________________________________________________________
  #   read the vector file and apply buffer if necessary                      ####


  if (shp_type == "vectfile") {

    if (is.null(buffer)) {
      # browser()
      # If no buffer, fInd the projection info using `gdalsrsinfo`. Read and reproj if needed,
      # otherwise do nothing
      mask_proj <- gdalUtils::gdalsrsinfo(mask_vect, as.CRS = T)@projargs
      if (mask_proj != rast_proj) {
        mask_vect <- read_shape(mask_vect) %>%
          sf::st_transform(rast_proj) %>%
          sf::st_combine() %>%
          # sf::st_sf(id = 1, .) %>%
          write_shape(temp_shapefile, overwrite = TRUE)
      } else {
        temp_shapefile <- mask_vect
      }

    } else {
      # If  buffer, Read, apply buffer and reproj if needed,
      # otherwise do nothing
      mask_vect <- read_shape(mask_vect) %>%
        sf::st_buffer(., buffer) %>%
        sf::st_combine() %>%
        sf::st_sf(id = 1, .)
      mask_proj <- sf::st_crs(mask_vect)$proj4string
      if (mask_proj != rast_proj) {
        mask_vect <- sf::st_transform(mask_vect, rast_proj)
      }
      write_shape(mask_vect, temp_shapefile, overwrite = TRUE)
    }
  } else {
    # input is a "*sp" or "sf" object
    if (shp_type == "spobject") {
      mask_vect <- sf::st_as_sf(mask_vect)
    }
    mask_vect <- mask_vect %>%
      sf::st_combine() %>%
      sf::st_sf(id = 1, .)

    if (!is.null(buffer)) {
      # apply buffer if needed
      mask_vect <- read_shape(mask_vect) %>%
        sf::st_buffer(buffer)
    }
    mask_proj <- sf::st_crs(mask_vect)$proj4string
    if (mask_proj != rast_proj) {
      mask_vect <- sf::st_transform(mask_vect, rast_proj)
    }
    write_shape(mask_vect, temp_shapefile, overwrite = TRUE)
  }

  #   ____________________________________________________________________________
  #   rasterize mask_vect - we can always use a BYte representation since the
  #   features of the input were combined                                     ####

  if (verbose) {(message("mask_rast --> Writing temporary rasterized shapefile"))}
  temp_rastermask <- tempfile(tmpdir = tempdir(), fileext = ".tiff")

  if (crop) {

    #   ____________________________________________________________________________
    #   find a correct cropping bounding box which allows to not "move" the corn####
    #    the corners while creating a vrt file (TODO --> Move to dedicated function !)

    rast_bbox <- raster::extent(in_rast)[c(1,3,2,4)]
    vect_bbox <- sf::st_bbox(mask_vect)

    col_coords <- rast_bbox[1] + raster::res(in_rast)[1] * seq_len(dim(in_rast)[2])
    row_coords <- rast_bbox[2] + raster::res(in_rast)[2] * seq_len(dim(in_rast)[1])
    start_x    <- ifelse((rast_bbox[1] < vect_bbox[1]),
                         col_coords[data.table::last(which(col_coords <= vect_bbox[1])) - 1],
                         rast_bbox[1])
    end_x      <- ifelse((rast_bbox[3] > vect_bbox[3]),
                         col_coords[data.table::last(which(col_coords <= vect_bbox[3])) + 1],
                         rast_bbox[3])
    start_y    <- ifelse((rast_bbox[2] < vect_bbox[2]),
                         row_coords[data.table::last(which(row_coords <= vect_bbox[2])) - 1],
                         rast_bbox[2])
    end_y      <- ifelse((rast_bbox[4] > vect_bbox[4]),
                         row_coords[data.table::last(which(row_coords <= vect_bbox[4])) + 1],
                         rast_bbox[4])
    te         <- c(start_x, start_y, end_x, end_y)

    if (in_rast[[1]]@file@name == "") {
      temprastfile <- tempfile(fileext = ".tif")
      raster::writeRaster(in_rast,
                          filename  = temprastfile,
                          options   = c("COMPRESS=DEFLATE"),
                          overwrite = TRUE)
      in_rast <- raster::raster(temprastfile)
    }

    # find which bands of the original raster were "passed"
    bands = list()
    for (bb in seq_len(raster::nlayers(in_rast))) {
      bands[[bb]] <- in_rast[[bb]]@data@band
    }
    bands = unlist(bands)

    temp_vrt        <- tempfile(fileext = ".vrt")
    buildvrt_string <- paste("-te ", paste(te, collapse = " "),
                             paste(paste("-b ", bands), collapse = " "),
                             temp_vrt,
                             in_rast[[1]]@file@name)

    system2(file.path(find_gdal(), "gdalbuildvrt"), args = buildvrt_string, stdout = NULL)

  } else {
    te <- raster::extent(in_rast)[c(1, 3, 2, 4)][] #- c(0, -res(in_rast)[1], res(in_rast)[1], 0)
  }
  #   ____________________________________________________________________________
  #   correct the extent for gdal_rasterize to get                            ####
  #   an identically sized output
  te <- te - c(0, -raster::res(in_rast)[1], raster::res(in_rast)[1], 0)
  # te <- raster::extent(in_rast)[c(1, 3, 2, 4)][] - c(0, -res(in_rast)[1], res(in_rast)[1], 0)

  #   ____________________________________________________________________________
  #   Rasterize the mask shapefile                                            ####

  temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-a_nodata", out_nodata,
                            "-te", paste(te, collapse = " "),
                            "-tr", paste(raster::res(in_rast), collapse = " "),
                            "-ot Byte",
                            "-tap",
                            temp_shapefile,
                            temp_rastermask)

  system2(file.path(find_gdal(), "gdal_rasterize"),
          args = rasterize_string, stdout = NULL)

  #   ____________________________________________________________________________
  #   apply the mask by multiplying the input raster by the mask, for each    ####
  #   band of the input

  if (!crop) {
    masked_rast <- in_rast * raster::raster(temp_rastermask)
  } else {
    masked_rast <- raster::stack(temp_vrt) * raster::raster(temp_rastermask)
  }

  if (to_file) {
    if (is.null(out_rast)) {
      out_rast = tempfile(fileext = ".tif")
    }
    raster::writeRaster(masked_rast, out_rast,
                options = c("COMPRESS=DEFLATE"),
                datatype = out_dt)
    masked_rast <- out_rast
  }

  return(masked_rast)
  #   ____________________________________________________________________________
  #   define clean-up                                                         ####

  on.exit(unlink(temp_shapefile))
  on.exit(unlink(temp_rastermask))

}
