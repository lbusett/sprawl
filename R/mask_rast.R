#' @title Mask a raster based on a vector
#' @description Masks a raster file or object on the basis of a vector file or object. Pixels not
#'   covered by the vector features are set to NoData. If the input raster is multi-band, the
#'   mask is automatically applied to all bands. An optional buffer can be applied to the input
#'   vector to allow a more "lenient" masking, or to remove also the borders of the vector.
#' @param in_rast Raster file or object inheriting class `raster` to be masked
#' @param mask Vector file or object of class `*sf` or `sp` to be used as a mask
#' @param crop `logical` if TRUE, `in_rast` is also cropped on the extent of `mask`,
#'   Default: FALSE
#' @param buffer `numeric` if not NULL, width of a buffer to be applied to `mask` before
#'   masking `in_rast`. If negative, mask is "reduced" prior to masking (see examples),
#'   Default: NULL
#' @param out_nodata `numeric` value to be assigned to areas outside the mask, Default: 'NoData'
#' @param to_file `logical` If TRUE, the output masked raster is save to file instead than sent back
#'   to the caller. In this case, the path to the saved file is returned instead
#' @param out_rast `character` filename where the masked raster should be saved (ignored if
#'   to_file == FALSE). If NULL while to_file == TRUE, the masked raster is saved on a temporaty
#'   file in the `R` temporary folder. The file is saved in TIFF format, with `DAFLATE` compression.
#' @param out_dt TO BE CHECKED !!!!
#' @param verbose `logical` if TRUE, extended processing information is sent to the console in the
#'   form of messages
#' @param verb_foreach `logical` DESCRIPTION
#' @return object of class `raster` (if to_file == FALSE), or `character` string corresponding to
#'   the filename of the created raster (if to_file == TRUE)
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
#' @importFrom rgdal ogrInfo
#' @importFrom foreach foreach %dopar%
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom raster raster extent writeRaster
#' @importFrom sf st_buffer st_crs st_transform st_as_sf st_combine st_sf
#' @importFrom parallel stopCluster

mask_rast <- function(in_rast,
                      mask,
                      crop       = FALSE,
                      buffer     = NULL,
                      out_nodata = NULL,
                      to_file    = FALSE,
                      out_rast   = NULL,
                      out_dt     = "FLT4S",
                      verbose    = TRUE,
                      verb_foreach = FALSE) {

  call <- as.list(match.call())
  message("mask_rast --> Masking: ", as.character(call[[2]]), " on: ", as.character(call[[3]]))

  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####


  # checks on in_rast ----

  in_rast   <- cast_rast(in_rast, "rastobj")
  rast_proj <- get_projstring(in_rast, abort = TRUE)
  rast_bbox <- get_extent(in_rast)

  # doubl check if the input raster is associated to a physical file (i.e., not "in memory")
  # If not, create a temporary physical file by saving in tempdir()

  if (in_rast[[1]]@file@name == "") {
    temprastfile <- tempfile(fileext = ".tif")
    raster::writeRaster(in_rast,
                        filename  = temprastfile,
                        options   = c("COMPRESS=DEFLATE"),
                        overwrite = TRUE)
    in_rast <- raster::brick(temprastfile)
  }

  # checks on mask ----
  mask      <- cast_vect(mask, "sfobject")
  mask_proj <- get_projstring(mask, abort = TRUE)

  #   ____________________________________________________________________________
  #   All checks passed - start working          ####

  #   ____________________________________________________________________________
  #   apply buffer to mask if necessary                                       ####
  temp_shapefile <- tempfile(fileext = ".shp")

  # if we have an sfobject now, reproject and buffer if necessary, then save as
  # shapefile
  #
  #   if (mask_type == "vectfile") {
  #     # if input is a vector file: if no buffer and no reproj, do nothing, otherwise
  #     # read the vector file and reset "mask_type" to "sfobject"
  #     if (is.null(buffer) & (rast_proj == mask_proj)) {
  #       vect_bbox <- rgdal::ogrInfo(mask, rgdal::ogrInfo(mask)$layer)$extent
  #       temp_shapefile <- mask
  #     } else {
  #       mask <- read_vect(mask)
  #       mask_type <- "sfobject"
  #     }
  #   }
  # if now we still have an sf object, buffer and reproject if necessary, then save
  # the mask to temp_shapefile
  # if (mask_type == "sfobject") {
  if (rast_proj != mask_proj) {
    mask <- mask %>%
      sf::st_transform(rast_proj)
  }
  if (!is.null(buffer)) {
    mask <- mask %>%
      sf::st_buffer(buffer)
  }
  mask %>%
    sf::st_combine() %>%
    sf::st_sf(id = 1, .) %>%
    write_shape(temp_shapefile, overwrite = TRUE)

  vect_bbox <- get_extent(mask)@extent
  # }
  #   ____________________________________________________________________________
  #   If crop ==TREU find a correct cropping bounding box which allows to not "move"
  #   the  corners while creating vrt files

  # te <- rast_bbox

  if (crop == TRUE) {

    col_coords <- rast_bbox@extent[1] + raster::res(in_rast)[1] * seq_len(dim(in_rast)[2])
    row_coords <- rast_bbox@extent[2] + raster::res(in_rast)[2] * seq_len(dim(in_rast)[1])

    # xmin
    if (rast_bbox@extent[1] < vect_bbox[1]) {
      rast_bbox@extent[1] <- col_coords[data.table::last(which(col_coords <= vect_bbox[1])) - 1]
    }
    # xmax
    if (rast_bbox@extent[3] > vect_bbox[3]) {
      rast_bbox@extent[3] <- col_coords[data.table::last(which(col_coords <= vect_bbox[3])) + 1]
    }
    # ymin
    if (rast_bbox@extent[2] < vect_bbox[2]) {
      rast_bbox@extent[2] <- row_coords[data.table::last(which(row_coords <= vect_bbox[2])) - 1]
    }
    # ymax
    if (rast_bbox@extent[4] > vect_bbox[4]) {
      rast_bbox@extent[4] <- row_coords[data.table::last(which(row_coords <= vect_bbox[4])) + 1]
    }
  }

  #   ____________________________________________________________________________
  #   Rasterize the mask shapefile: allows great improvements in speed        ####
  #   on large raster. Use gdal_rasterize instaad than raster::rasterize to
  #   create the rasterized shapefile much faster and save as Byte

  # te <- te - c(-raster::res(in_rast)[1], -raster::res(in_rast)[1], 0,  0)
  # te <- raster::extent(in_rast)[c(1, 3, 2, 4)][] - c(0, -res(in_rast)[1], res(in_rast)[1], 0)
  #

  # write_shape(mask, temp_shapefile, overwrite = TRUE)
  #
  if (verbose) {
    message("mask_rast --> Rasterizing the vector map to a temporary TIFF file")
  }
  temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-te", paste(rast_bbox@extent, collapse = " "),
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
  clust    <- sprawl_initcluster(in_rast)
  tempfold <- file.path(tempdir(), "tests", paste0("sprawlmask_",sample(1:1000))[1])
  dir.create(tempfold, showWarnings = FALSE, recursive = TRUE)

  if (verbose) {
    message("mask_rast --> Masking data from ", clust$opts$n_bands, " bands - Please wait !")
    pb       <- utils::txtProgressBar(max = clust$opts$n_bands, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts     <- list(progress = progress)
  } else {
    opts = list()
  }

  temp_tiffs <- foreach::foreach(
    band = clust$opts$bands,
    .combine      = "c",
    .verbose      = FALSE,
    .packages     = c("raster", "sprawl"),
    .options.snow = opts
  ) %dopar% {

    tempout  <- file.path(tempfold,paste0("sprawlmask_b",
                                          sprintf("%03i", band),
                                          ".tif"))

    #   ____________________________________________________________________________
    #   create a temporary vrt file corresponding to the band to be preocesse   ####
    #   This allows flexibility in the case that a stack is passed containing
    #   coming from different files

    infile    <- in_rast[[band]]@file@name
    temp_vrt  <- tempfile(fileext = ".vrt")
    buildvrt_string <- paste("-te ", paste(rast_bbox@extent, collapse = " "), temp_vrt, infile)
    system2(file.path(find_gdal(), "gdalbuildvrt"),
            args = buildvrt_string,
            stdout = NULL)
    in_rast <- raster::brick(temp_vrt)


    # Now launch the masking between the raster vrt and the
    # temporary rasterized mask

    if (is.null(out_nodata)) {

      raster::mask(in_rast[[band]],
                   maskvalue = 0,
                   raster::brick(temp_rastermask),
                   filename  = tempout,
                   options   = c("COMPRESS=DEFLATE"),
                   overwrite = TRUE)

    } else {
      raster::mask(in_rast[[band]],
                   maskvalue = 0,
                   raster::brick(temp_rastermask),
                   filename  = tempout,
                   options   = c("COMPRESS=DEFLATE"),
                   overwrite = TRUE,
                   NAflag    = out_nodata)
    }

    return(tempout)
  }

  parallel::stopCluster(clust$clust)

  #   ____________________________________________________________________________
  #   end processing: recast if needed and return                             ####

  if (to_file == FALSE) {
    if (clust$opts$n_bands == 1) {
      return(raster::raster(temp_tiffs))
    } else {
      return(raster::stack(temp_tiffs))
    }
  } else {
    tempvrt <- tempfile(fileext = ".vrt")
    gdalUtils::gdalbuildvrt(gdalfile = temp_tiffs, output.vrt = tempvrt, separate = T)
    if (is.null(out_rast)) {
      return(tempvrt)
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
      return(out_rast)
    }
  }

  #   ____________________________________________________________________________
  #   define clean-up                                                         ####
  on.exit(unlink(temp_shapefile))
  on.exit(unlink(temp_rastermask))
  on.exit(unlink(temprastfile))
  return(masked_out)
}
