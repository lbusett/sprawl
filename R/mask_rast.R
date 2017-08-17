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
#' @param out_type `character`
#'   - if == "rastobj", return a `Raster` object;
#'   - if == "filename", return the filename of the masked layer (GTiff or gdal vrt format
#'   depending on other arguments - see below)
#'
#'   Default: "rastobj" (If an invalid string is provided, defaults to `rastobj`)
#' @param out_basename `character` filename where the masked raster should be saved (ignored if
#'   to_file == FALSE). If NULL, the masked raster is saved on a temporaty file in the `R`
#'   temporary folder, named `<basename(in_rast)>_masked.tif`:. The file is saved in TIFF format, with `DEFLATE` compression. For
#'   multiband inputs, a separate file is saved for each band (e.g., out_basename_001. tif,
#'   out_basename_002. tif...), unless the `save_multiband` argument is set to "TRUE"
#' @param save_multiband `logical`
#'   - if TRUE, and `in_rast` is multi-band, the single-band masked layers are saved as a
#'   single multiband tiff at the end of processing, and the single-band files are deleted;
#'   - if FALSE, and `in_rast` is multi-band, a gdal vrt file pointing to all masked bands
#'     is created instead.
#'
#'   Default: FALSE
#' @param out_dtype `character` data type of the output masked files, according to gdal
#'   specifications for Gtiff files ("Byte", "UInt16", "Int16", "UInt32", "Int32", "Float32",
#'   "Float64", "CInt16", "CInt32", "CFloat32" and "CFloat64"). If NULL, the data type
#'   is retrieved from the input, Default: NULL
#' @param overwrite `logical` if TRUE, and out_basename is set and existing, existing files
#'   are overwritten, Default: FALSE
#' @param verbose `logical` if TRUE, extended processing information is sent to the console
#'  in the form of messages
#' @param verb_foreach `logical` allow verbose output from `foreach` for debugging purposes,
#'   Default: FALSE
#' @return object of class `raster` (if out_type == `rastobj`), or `character` string
#'   corresponding to the filename of the created raster (if out_type == `filename` )
#' @examples
#' \dontrun{
#' library(sprawl)
#' library(sprawl.data)
#' library(raster)
#' in_polys <- read_vect(system.file("extdata","lc_polys.shp", package = "sprawl.data"),
#'                        stringsAsFactors = T)
#' in_rast  <- raster::stack(system.file("extdata", "sprawl_EVItest.tif",
#'                        package = "sprawl.data"))[[1]]
#' in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
#' masked   <- mask_rast(in_rast, in_polys, verbose = FALSE)
#' plot_rast(in_rast, in_poly = in_polys)
#' plot_rast(masked, in_poly = in_polys)
#' }
#' @rdname mask_rast
#' @export
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#' @importFrom dplyr case_when
#' @importFrom rgdal ogrInfo
#' @import foreach
#' @importFrom foreach "%dopar%"
#' @importFrom gdalUtils gdalsrsinfo
#' @importFrom raster raster extent writeRaster
#' @importFrom sf st_buffer st_crs st_transform st_as_sf st_combine st_sf
#' @importFrom parallel stopCluster

mask_rast <- function(in_rast,
                      mask,
                      crop           = FALSE,
                      buffer         = NULL,
                      out_nodata     = NULL,
                      out_type       = "rastobj",
                      out_basename   = NULL,
                      save_multiband = FALSE,
                      out_dtype      = NULL,
                      overwrite      = FALSE,
                      verbose        = TRUE,
                      verb_foreach   = FALSE) {

  call <- as.list(match.call())
  message("mask_rast --> Masking: ",
          as.character(call[[2]]), " on: ",
          as.character(call[[3]]))

  #   __________________________________________________________________________
  #   Check the arguments                                                   ####

  # checks on in_rast ----
  in_rast   <- cast_rast(in_rast, "rastobject")
  rast_proj <- get_projstring(in_rast, abort = TRUE)
  rast_bbox <- get_extent(in_rast)@extent
  bnames_in <- names(in_rast)
  times_in  <- raster::getZ(in_rast)
  n_bands   <- raster::nlayers(in_rast)
  if (is.null(out_dtype)) {
    if (n_bands == 1 | (class(in_rast) == "RasterBrick")) {
      in_dtype <- in_rast@file@datanotation
    } else {
      in_dtype <- in_rast@layers[[1]]@file@datanotation
    }
    dtype   <- convert_rastdtype(in_dtype, "raster")
  } else {
    dtype   <- convert_rastdtype(out_dtype, "gdal")
  }

  # double check if the input raster is associated to a physical file (i.e.,
  # not "in memory"). If not, create a temporary physical file by saving in
  # tempdir()

  if (in_rast[[1]]@file@name == "") {
    temprastfile <- tempfile(fileext = ".tif")
    raster::writeRaster(in_rast,
                        filename  = temprastfile,
                        options   = c("COMPRESS=DEFLATE"),
                        overwrite = overwrite)
    in_rast <- raster::brick(temprastfile)
  }

  # checks on mask ----
  mask      <- cast_vect(mask, "sfobject")
  mask_proj <- get_projstring(mask, abort = TRUE)


  #   ____________________________________________________________________________
  #   set the output folder (in tempdir if out_basename == NULL)              ####
  if (is.null(out_basename)){
    outfold      <- file.path(tempdir(), paste0("sprawlmask_",sample(1:1000))[1])
    out_basename <- file.path(outfold, "sprawlmask")
  } else {
    outfold <- dirname(out_basename )
  }
  dir.create(outfold, showWarnings = FALSE, recursive = TRUE)

  #   __________________________________________________________________________
  #   apply buffer to mask if necessary                                     ####
  temp_shapefile <- tempfile(fileext = ".shp")
  if (rast_proj != mask_proj) {
    mask <- mask %>%
      sf::st_transform(rast_proj)
  }
  if (!is.null(buffer)) {
    mask <- mask %>%
      sf::st_buffer(buffer)
  }

  #   __________________________________________________________________________
  #   Combine all features in one to speed up rasterization and compute bbox####
  mask %>%
    sf::st_combine() %>%
    sf::st_sf(id = 1, .) %>%
    write_shape(temp_shapefile, overwrite = TRUE)

  vect_bbox <- get_extent(mask)@extent

  #   __________________________________________________________________________
  #   If crop ==TRUE find a correct cropping bounding box which allows to   ####
  #   not "move" the  corners while creating vrt files

  if (crop == TRUE) {

    col_coords <- rast_bbox[1] +
      raster::res(in_rast)[1] * seq_len(dim(in_rast)[2])

    row_coords <- rast_bbox[2] +
      raster::res(in_rast)[2] * seq_len(dim(in_rast)[1])

    # xmin
    if (rast_bbox[1] < vect_bbox[1]) {
      first_col    <- data.table::last(which(col_coords <= vect_bbox[1]))
      rast_bbox[1] <- col_coords[first_col - 1]
    }

    # ymin
    if (rast_bbox[2] < vect_bbox[2]) {
      first_row    <- data.table::last(which(row_coords <= vect_bbox[2]))
      rast_bbox[2] <- row_coords[first_row - 1]
    }

    # xmax
    if (rast_bbox[3] > vect_bbox[3]) {
      last_col     <- data.table::last(which(col_coords <= vect_bbox[3]))
      rast_bbox[3] <- col_coords[last_col + 1]
    }

    # ymax
    if (rast_bbox[4] > vect_bbox[4]) {
      last_row     <- data.table::last(which(row_coords <= vect_bbox[4]))
      rast_bbox[4] <- row_coords[last_row + 1]
    }
  }

  #   __________________________________________________________________________
  #   Rasterize the mask shapefile: allows great improvements in speed      ####
  #   on large raster. Use gdal_rasterize instaad than raster::rasterize to
  #   create the rasterized shapefile much faster and save as Byte

  if (verbose) {
    message("mask_rast --> Rasterizing the vector map to a temporary TIFF file")
  }
  temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-te", paste(rast_bbox, collapse = " "),
                            "-tr", paste(raster::res(in_rast), collapse = " "),
                            "-ot Byte",
                            temp_shapefile,
                            temp_rastermask)

  system2(file.path(find_gdal(), "gdal_rasterize"),
          args = rasterize_string, stdout = NULL)

    # ____________________________________________________________________________
  # create a temporary vrt file corresponding to the band to be preocessed  ####
  # This allows flexibility in the case that a stack is passed containing
  # coming from different files
  #
  # If the bands in the original stack are not all coming
  # from the same on-disk raster, build a txt file to tell gdalbuildvrt
  # which band comes from where !

  temp_vrt        <- tempfile(fileext = ".vrt")

  if (raster::nlayers(in_rast) > 1) {

    bands <- list()
    files <- list()

    for (bb in seq_len(raster::nlayers(in_rast))) {

      bands[[bb]] <- in_rast@layers[[bb]]@data@band
      files[[bb]] <- in_rast[[bb]]@file@name
    }
    bands <- unlist(bands)
    files <- unlist(files)

    if (length(unique(files)) > 1) {

      tmp_txt <- tempfile(fileext = ".txt")
      writeLines(files, tmp_txt)
      buildvrt_string <- paste("-te ", paste(rast_bbox, collapse = " "),
                               "-separate",
                               "-input_file_list",
                               tmp_txt,
                               temp_vrt)
    } else {
      buildvrt_string <- paste("-te ", paste(rast_bbox, collapse = " "),
                               paste(paste("-b ", bands), collapse = " "),
                               temp_vrt,
                               in_rast[[1]]@file@name)
    }

  } else {
    infile          <- in_rast@file
    buildvrt_string <- paste("-te ",
                           paste(rast_bbox, collapse = " "),
                           temp_vrt,
                           infile)
  }

  system2(file.path(find_gdal(), "gdalbuildvrt"),
          args = buildvrt_string,
          stdout = NULL)

  in_rast         <- raster::brick(temp_vrt)

  #   ____________________________________________________________________________
  #   Compute the mask by band - this allows to use parallelized execution !  ####
  #   + save each band as a compressed tiff: this allows not wasting space
  #   and successively create small compressed multiband files

  if (verbose) {
    message("mask_rast --> Masking bands")
  }
  clust    <- sprawl_initcluster(in_rast)

  if (verbose) {
    message("mask_rast --> Masking data from ", n_bands, " bands - Please wait !")
    pb       <- utils::txtProgressBar(max = n_bands, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts     <- list(progress = progress)
  } else {
    opts = list()
  }
  # browser()
  out_tiffs <- foreach::foreach(
    band = clust$opts$bands,
    .combine      = "c",
    .verbose      = verb_foreach,
    .packages     = c("raster", "sprawl", "tools"),
    .options.snow = opts
  ) %dopar% {

    # if (is.null(out_basename)) {
    #
    #   if (n_bands == 1) {
    #     tiffout  <- file.path(outfold, "sprawlmask.tif")
    #   } else {
    #     bands_fold <- file.path(outfold, "sprawlmask_bands")
    #     dir.create(bands_fold, recursive = TRUE, showWarnings = FALSE)
    #     tiffout   <- file.path(bands_fold, paste0("sprawlmask_b",
    #                                               sprintf("%03i", band),".tif"))
    #   }
    # } else {

    basename_noext <- paste0(basename(tools::file_path_sans_ext(out_basename)))
    if (n_bands == 1) {
      tiffout  <- file.path(outfold, paste0(basename_noext, ".tif"))
    } else {
      bands_fold <- file.path(outfold, paste0(basename_noext,"_bands"))
      dir.create(bands_fold, recursive = TRUE, showWarnings = FALSE)
      tiffout    <- file.path(bands_fold, paste0(basename_noext, "_",
                                                 sprintf("%03i", band), ".tif"))
    }

    #   ____________________________________________________________________________
    #   create a temporary vrt file corresponding to the band to be preocessed   ####
    #   This allows flexibility in the case that a stack is passed containing
    #   coming from different files
#
#     infile          <- in_rast[[band]]@file@name
#     temp_vrt        <- tempfile(fileext = ".vrt")
#     buildvrt_string <- paste("-te ",
#                              paste(rast_bbox, collapse = " "),
#                              temp_vrt,
#                              infile)
#
#     system2(file.path(find_gdal(), "gdalbuildvrt"),
#             args = buildvrt_string,
#             stdout = NULL)
#
#     temp_rast         <- raster::brick(temp_vrt)

    #   ____________________________________________________________________________
    #   Now launch the masking between the raster vrt and the                   ####
    #   temporary rasterized mask

    if (is.null(out_nodata)) {

      raster::mask(in_rast[[band]],
                   maskvalue = 0,
                   raster::brick(temp_rastermask),
                   filename  = tiffout,
                   options   = c("COMPRESS=LZW"),
                   overwrite = TRUE,
                   data_type = dtype[["raster"]])

    } else {
      raster::mask(temp_rast,
                   maskvalue = 0,
                   raster::brick(temp_rastermask),
                   filename  = tiffout,
                   options   = c("COMPRESS=LZW"),
                   overwrite = TRUE,
                   data_type = dtype[["raster"]],
                   NAflag    = out_nodata)
    }

    return(tiffout)
  }

  parallel::stopCluster(clust$clust)

  #   ____________________________________________________________________________
  #   end processing: recast if needed and return                             ####


  #   ____________________________________________________________________________
  #   if save_multiband == TRUE, save all the bands as a sibngle tiff         ####

  if (n_bands == 1) {
    out_file <- out_tiffs
  } else {

    out_file <- paste0(tools::file_path_sans_ext(out_basename), ".vrt")
    gdalUtils::gdalbuildvrt(out_tiffs,
                            out_file,
                            separate  = TRUE,
                            overwrite = TRUE)

    if (save_multiband == TRUE) {

      out_vrt <- out_file
      out_file <- paste0(tools::file_path_sans_ext(out_basename), ".tif")

      if (is.null(out_nodata)) {
        gdalUtils::gdal_translate(out_vrt,
                                  out_file,
                                  ot        = dtype[["gdal"]],
                                  co        = "COMPRESS=None",
                                  overwrite = overwrite)
      } else {
        gdalUtils::gdal_translate(out_vrt,
                                  out_file,
                                  ot        = dtype[["gdal"]],
                                  a_nodata  = out_nodata,
                                  co        = "COMPRESS=None",
                                  overwrite = overwrite)
      }

    }

  }

  if (out_type == "rastobj") {

    out_obj <- raster::brick(out_file)
    names(out_obj) <- bnames_in
    if (!is.null(times_in)) {
      out_obj <- raster::setZ(out_obj, times_in)
    }
    return(out_obj)

  } else {
    return(out_file)
  }

  #   raster::writeRaster(out_obj,
  #                       filename = ifelse(
  #                         is.null(out_basename),
  #                         file.path(outfold, "sprawlmask.tif"),
  #                         paste0(file_path_sans_ext(out_basename), ".tif")
  #                       ), options   = c("COMPRESS=DEFLATE"),
  #                       overwrite = overwrite)
  # } else {
  #   raster::writeRaster(out_obj,
  #                       filename = ifelse(
  #                         is.null(out_basename),
  #                         file.path(outfold, "sprawlmask.tif"),
  #                         paste0(file_path_sans_ext(out_basename), ".tif")
  #                       ), options   = c("COMPRESS=DEFLATE"),
  #                       overwrite = overwrite,
  #                       NAflag    = out_nodata)
  # }
  # }


  # if (n_bands == 1) {
  #   out_obj <- raster::raster(out_tiffs)
  # } else {
  #   out_obj <- raster::stack(out_tiffs)
  # }
  #
  # if (to_file == FALSE) {
  #   if (n_bands == 1) {
  #     out_obj <- raster::raster(out_tiffs)
  #   } else {
  #     out_obj <- raster::stack(out_tiffs)
  #   }
  #   names(out_obj) <- bnames_in
  #   if (!is.null(times_in)) {
  #     out_obj <- raster::setZ(out_obj, times_in)
  #   }
  #   if (!is.null(out_rast)) {
  #     if (is.null(out_nodata)) {
  #       raster::writeRaster(out_obj,
  #                           filename = out_rast,
  #                           options   = c("COMPRESS=DEFLATE"),
  #                           overwrite = overwrite)
  #     } else {
  #       raster::writeRaster(out_obj,
  #                           filename = out_rast,
  #                           options   = c("COMPRESS=DEFLATE"),
  #                           NAflag    = out_nodata,
  #                           overwrite = overwrite)
  #     }
  #
  #   }
  #
  # } else {
  #   tempvrt <- tempfile(fileext = ".vrt")
  #   gdalUtils::gdalbuildvrt(gdalfile = out_tiffs, output.vrt = tempvrt, separate = T)
  #   if (is.null(out_rast)) {
  #     return(tempvrt)
  #   } else {
  #     if (is.null(out_nodata)) {
  #       gdalUtils::gdal_translate(tempvrt,
  #                                 out_rast,
  #                                 options   = c("COMPRESS=DEFLATE"))
  #
  #     } else {
  #       gdalUtils::gdal_translate(tempvrt,
  #                                 out_rast,
  #                                 options   = c("COMPRESS=DEFLATE"),
  #                                 NAflag    = out_nodata)
  #     }
  #     return(out_rast)
  #   }
  # }

  #   ____________________________________________________________________________
  #   define clean-up                                                         ####
  on.exit(unlink(temp_shapefile))
  on.exit(unlink(temp_rastermask))
  on.exit({if (exists("temprastfile")) unlink(temprastfile)})
  # return(out_obj)
}
