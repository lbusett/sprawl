#' @title Mask a raster based on a vector
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
#' in_polys <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"),
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
#' @importFrom foreach foreach %dopar%
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom raster raster extent writeRaster
#' @importFrom sf st_buffer st_crs st_transform st_as_sf st_combine st_sf

mask_rast <- function(in_rast,
                      mask_vect,
                      crop       = FALSE,
                      buffer     = NULL,
                      out_nodata = NULL,
                      to_file    = FALSE,
                      out_rast   = NULL,
                      out_dt     = "FLT4S",
                      verbose    = TRUE,
                      verb_foreach = FALSE) {
  # browser()
  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####
  ras_type  <- check_spatype(in_rast)
  mask_type <- check_spatype(mask_vect)

  rast_proj <- get_projstring(in_rast)
  mask_proj <- get_projstring(mask_vect)

  # checks on in_rast
  #
  # in_rast <- cast_rast(in_rast, "rastobject", abort_on_none = TRUE)

  if (check_spatype(in_rast) == "rastfile") {
    in_rast  <- raster::raster(in_rast)
    ras_type <- "rastobject"
  }
  if (ras_type != "rastobject") {
    stop("mask_rast --> `in_rast` must be a `*Raster` object or raster file name. Aborting !")
  }

  # check if the input raster is associated to a physical file (i.e., not "in memory)
  # If not, create a temporary physical file by saving in tempdir()

  if (in_rast[[1]]@file@name == "") {
    temprastfile <- tempfile(fileext = ".tif")
    raster::writeRaster(in_rast,
                        filename  = temprastfile,
                        options   = c("COMPRESS=DEFLATE"),
                        overwrite = TRUE)
    in_rast <- raster::stack(temprastfile)
  }

  # checks on mask_vect

  if (mask_type == "none") {
    stop("mask_rast --> ", basename(mask_vect), "is not a vector file or object. Aborting !" )
  }

  if (ras_type == "rastfile") {
    rastname = in_rast
  } else {
    rastname = in_rast[[1]]
    rastname = rastname@file@name
  }

  temp_shapefile <- tempfile(fileext = ".shp")
  #   ____________________________________________________________________________
  #   read the vector file and apply buffer if necessary                      ####

  if (mask_type == "vectfile") {

    if (is.null(buffer)) {

      # If no buffer, fInd the projection info using `gdalsrsinfo`. Read and reproj if needed,
      # otherwise do nothing

      if (mask_proj != rast_proj) {

        mask_vect <- read_vect(mask_vect) %>%
          sf::st_transform(rast_proj) %>%
          sf::st_combine() %>%
          sf::st_sf(id = 1, .) %>%
          write_shape(temp_shapefile, overwrite = TRUE)

      } else {
        temp_shapefile <- mask_vect
      }

    } else {
      # If  buffer, Read, apply buffer and reproj if needed,
      # otherwise do nothing
      mask_vect <- read_vect(mask_vect) %>%
        sf::st_buffer(., buffer) %>%
        sf::st_combine() %>%
        sf::st_sf(id = 1, .)
      # mask_proj <- sf::st_crs(mask_vect)$proj4string
      if (mask_proj != rast_proj) {
        mask_vect <- sf::st_transform(mask_vect, rast_proj)
      }
      write_shape(mask_vect, temp_shapefile, overwrite = TRUE)
      # mask_vect <- read_vect(temp_shapefile) %>%
      #   as("Spatial")
    }
  } else {
    # input is a "*sp" or "sf" object
    if (mask_type == "spobject") {
      mask_vect <- sf::st_as_sf(mask_vect)
    }
    mask_vect <- mask_vect %>%
      sf::st_combine() %>%
      sf::st_sf(id = 1, .)

    if (!is.null(buffer)) {
      # apply buffer if needed
      mask_vect <- read_vect(mask_vect) %>%
        sf::st_buffer(buffer)
    }
    # mask_proj <- get_projstring(mask_vect)
    if (mask_proj != rast_proj) {
      mask_vect <- sf::st_transform(mask_vect, rast_proj)
    }
    write_shape(mask_vect, temp_shapefile, overwrite = TRUE)
    # mask_vect <- mask_vect %>%
    #   as("Spatial")
  }

  if (crop == TRUE) {

    #   ____________________________________________________________________________
    #   find a correct cropping bounding box which allows to not "move" the corn####
    #    the corners while creating a vrt file (TODO --> Move to dedicated function !)

    rast_bbox <- raster::extent(in_rast)[c(1,3,2,4)]
    vect_bbox <- sf::st_bbox(mask_vect)
    # vect_bbox <- raster::extent(mask_vect)[c(1,3,2,4)]

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

  } else {
    te <- raster::extent(in_rast)[c(1,3,2,4)]
  }
  #   ____________________________________________________________________________
  #   Rasterize the mask shapefile: allows great improvements in speed        ####
  #   on large raster. Use gdal_rasterize instaad than raster::rasterize to
  #   create the rasterized shapefile much faster and save as Byte

  # te <- te - c(-raster::res(in_rast)[1], -raster::res(in_rast)[1], 0,  0)
  # te <- raster::extent(in_rast)[c(1, 3, 2, 4)][] - c(0, -res(in_rast)[1], res(in_rast)[1], 0)
  #

  # write_shape(mask_vect, temp_shapefile, overwrite = TRUE)
  #
  if (verbose) {
    message("mask_rast --> Rasterizing the vector map to a temporary TIFF file")
  }
  temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-te", paste(te, collapse = " "),
                            "-tr", paste(raster::res(in_rast), collapse = " "),
                            "-ot Byte",
                            temp_shapefile,
                            temp_rastermask)

  system2(file.path(find_gdal(), "gdal_rasterize"),
          args = rasterize_string, stdout = NULL)


  #   ____________________________________________________________________________
  #   Compute the mask by band - this allows to use parallelized execution !  ####
  #   + save each band as a compressed tiff: this allows not wasting space
  #   and successively create small compressed multiband files

  if (verbose) {
    message("mask_rast --> Masking bands")
  }
  clust = sprawl_initcluster(in_rast)
  tempfold <- file.path(tempdir(), "tests",paste0("sprawlmask_",sample(1:1000))[1])
  dir.create(tempfold, showWarnings = FALSE, recursive = TRUE)

  if (verbose) {
    message("mask_rast --> Masking data from ", clust$opts$n_bands, " bands - Please wait !")
    pb       <- utils::txtProgressBar(max = clust$opts$n_bands, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts     <- list(progress = progress)
  } else {
    opts = list()
  }

  temp_tiffs <- foreach::foreach(band = clust$opts$bands,
                                 # temp_tiffs <- foreach::foreach(band = 1:2,
                                 .combine      = "c",
                                 .verbose      = FALSE,
                                 .packages     = c("raster", "sprawl"),
                                 .options.snow = opts) %dopar% {
                                   tempout  <- file.path(tempfold,
                                                         paste0("sprawlmask_b",
                                                                sprintf("%03i", band), ".tif"))

                                   #   ____________________________________________________________________________
                                   #   create a temporary vrt file corresponding to the band to be preocesse                           ####
                                   #   This allows flexibility in the case that a stack is passed containing
                                   #   coming from different files

                                   infile    <- in_rast[[band]]@file@name
                                   temp_vrt  <- tempfile(fileext = ".vrt")
                                   buildvrt_string <- paste("-te ", paste(te, collapse = " "),
                                                            temp_vrt,
                                                            infile)
                                   system2(file.path(find_gdal(), "gdalbuildvrt"), args = buildvrt_string, stdout = NULL)
                                   in_rast <- raster::brick(temp_vrt)


                                   # Now launch the masking between the raster vrt and the
                                   # temporary rasterized mask

                                   if (is.null(out_nodata)) {

                                     test <-  raster::mask(in_rast[[band]],
                                                           brick(temp_rastermask),
                                                           filename  = tempout,
                                                           options   = c("COMPRESS=DEFLATE"),
                                                           overwrite = TRUE)

                                   } else {
                                     test <-  raster::mask(in_rast[[band]],
                                                           brick(temp_rastermask),
                                                           filename  = tempout,
                                                           options   = c("COMPRESS=DEFLATE"),
                                                           overwrite = TRUE,
                                                           NAflag    = out_nodata)
                                   }

                                   return(tempout)
                                 }

  parallel::stopCluster(clust$clust)

  masked_out <- raster::stack(temp_tiffs)

  if (to_file) {

    tempvrt <- tempfile(fileext = ".vrt")
    gdalUtils::gdalbuildvrt(gdalfile = temp_tiffs, output.vrt = tempvrt,
                            separate = T)

    if (is.null(out_rast)) {

      masked_out <- tempvrt

    } else {
      if (is.null(out_nodata)) {
        gdalUtils::gdal_translate(tempvrt,
                                  out_rast,
                                  options   = c("COMPRESS=DEFLATE"))

      } else {
        gdalUtils::gdal_translate(tempvrt,
                                  out_rast,
                                  options   = c("COMPRESS=DEFLATE"),
                                  NAflag    = out_nodata)
      }
      masked_out <- out_rast
    }
  }

  #   ____________________________________________________________________________
  #   define clean-up                                                         ####
  on.exit(unlink(temp_shapefile))
  return(masked_out)

}
