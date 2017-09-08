#' @title Mask a raster based on a vector
#' @description Masks a raster file or object on the basis of a vector file or object.
#'   Pixels not covered by the vector features are set to NoData. If the input raster
#'   is multi-band, the mask is automatically applied to all bands. An optional
#'   buffer can be applied to the input vector to allow a more "lenient" masking,
#'   or to remove also the borders of the vector.
#' @param in_rast Raster file or object inheriting class `raster` to be masked
#' @param mask Vector file or object of class `*sf` or `sp` to be used as a mask
#' @param crop `logical` if TRUE, `in_rast` is also cropped on the extent of `mask`,
#'   Default: FALSE
#' @param buffer `numeric` if not NULL, width of a buffer to be applied to `mask`
#'   before masking `in_rast`. If negative, mask is "reduced" prior to masking
#'   (see examples), Default: NULL
#' @param out_nodata `numeric` value to be assigned to areas outside the mask,
#'   Default: 'NoData'
#' @param out_type `character`
#'   - if == "rastobj", return a `Raster` object;
#'   - if == "filename", return the filename of the masked layer (GTiff or gdal vrt format
#'   depending on other arguments - see below)
#'   Default: "rastobj" (If an invalid string is provided, defaults to `rastobj`)
#' @param out_filename `character` filename where the masked raster should be saved
#'   If NULL, the masked raster is saved on a temporary file in the `R` temporary
#'   folder, named `<basename(in_rast)>_sprawlmask.tif`. The file is saved in GTtiff
#'   format, with `compress` compression.
#' @param out_dtype `character` data type of the output masked files, according
#'   to gdal specifications for Gtiff files ("Byte", "UInt16", "Int16", "UInt32",
#'   "Int32", "Float32", "Float64", "CInt16", "CInt32", "CFloat32" and "CFloat64").
#'   If NULL, the data type is retrieved from the input, Default: NULL
#' @param compress `logical` allow verbose output from `foreach` for debugging
#'   purposes, Default: FALSE
#' @param overwrite `logical` if TRUE, and out_filename is set and existing,
#'   existing files are overwritten, Default: FALSE
#' @param parallel `logical` if TRUE, use ClusterR to implement multicore
#'   processing. This speeds up execution for large rasters, Default: FALSE
#' @param verbose `logical` if TRUE, extended processing information is sent to
#'  the console in the form of messages
#'
#' @return object of class `raster` (if out_type == `rastobj`), or `character`
#'   string corresponding to the filename of the created raster (if out_type ==
#'   `filename`)
#' @examples
#' \dontrun{
#' library(sprawl)
#' library(sprawl.data)
#' library(raster)
#' in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
#'                                   package = "sprawl.data"),
#'                        stringsAsFactors = T)
#' in_rast  <- raster::stack(system.file("extdata/MODIS_test", "EVIts_test.tif",
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
#' @importFrom raster getZ writeRaster brick mask beginCluster clusterR
#'  endCluster
#' @importFrom sf st_transform st_buffer st_combine st_sf
#' @importFrom magrittr "%>%"

mask_rast <- function(in_rast,
                      mask,
                      crop         = FALSE,
                      buffer       = NULL,
                      out_type     = "rastobject",
                      out_filename = NULL,
                      out_dtype    = NULL,
                      out_nodata   = NULL,
                      compress     = "None",
                      overwrite    = FALSE,
                      parallel     = FALSE,
                      verbose      = TRUE) {

  # to avoid NOTE on check
  . <- NULL

  call <- as.list(match.call())
  message("mask_rast --> Masking: ",
          as.character(call[[2]]), " on: ",
          as.character(call[[3]]))

  #   __________________________________________________________________________
  #   Check the arguments                                                   ####

  # checks on in_rast ----

  in_rast   <- cast_rast(in_rast, "rastobject")
  rastinfo  <- get_rastinfo(in_rast)
  rast_proj <- get_proj4string(in_rast)
  bnames_in <- names(in_rast)
  times_in  <- raster::getZ(in_rast)
  if (is.null(out_dtype)) {
    in_dtype <- rastinfo$dtype
    dtype    <- convert_rastdtype(in_dtype, "raster")
  } else {
    dtype    <- convert_rastdtype(out_dtype, "gdal")
  }

  # double check if the input raster is associated to a physical file (i.e.,
  # not "in memory"). If not, create a temporary physical file by saving in
  # tempdir()

  if (rastinfo$fnames[[1]] == "") {
    temprastfile <- tempfile(fileext = ".tif")
    raster::writeRaster(in_rast,
                        filename  = temprastfile,
                        options   = c("COMPRESS=DEFLATE"),
                        overwrite = overwrite)
    in_rast <- raster::brick(temprastfile)
  }

  # checks on mask ----
  mask      <- cast_vect(mask, "sfobject")
  mask_proj <- get_proj4string(mask, abort = TRUE)

  #   __________________________________________________________________________
  #   set the output folder (in tempdir if out_filename == NULL)            ####
  if (is.null(out_filename)){
    outfold      <- file.path(tempdir(), paste0("sprawlmask_",sample(1:1000))[1]) #nolint
    out_filename <- file.path(outfold, "sprawlmask.tif")
  } else {
    outfold <- dirname(out_filename )
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

  #   __________________________________________________________________________
  #   If crop ==TRUE create a vrt file corresponding to the cropped raster  ####

  if (crop) {
    in_rast <- crop_rast(in_rast, mask, out_type = "vrtfile") %>%
      raster::brick()
  }
  #   __________________________________________________________________________
  #   Rasterize the mask shapefile: allows great improvements in speed      ####
  #   on large raster. Use gdal_rasterize instaad than raster::rasterize to
  #   create the rasterized shapefile much faster and save as Byte

  if (verbose) {
    message("mask_rast --> Rasterizing the vector mask to a temporary TIFF",
            "file")
  }
  temp_rastermask  <- tempfile(tmpdir = tempdir(), fileext = ".tif")
  rasterize_string <- paste("-at",
                            "-burn 1",
                            "-a_nodata 0",
                            "-te", paste(get_extent(in_rast)@extent, collapse = " "), #nolint
                            "-tr", paste(rastinfo$res, collapse = " "),
                            "-ot Byte",
                            temp_shapefile,
                            temp_rastermask)

  system2(file.path(find_gdal(), "gdal_rasterize"),
          args = rasterize_string, stdout = NULL)

  #   __________________________________________________________________________
  #   Compute the mask - if parallel = TRUE, initialize a Cluster and use   ####
  #   raster::ClusterR(), otherwise use raster::overlay() !

  if (verbose) {
    message("mask_rast --> Masking bands")
  }

  if (!parallel) {
    masked_out <- raster::mask(
      in_rast,
      raster::brick(temp_rastermask),
      filename    = out_filename,
      options     = paste0("COMPRESS=", compress),
      overwrite   = TRUE,
      data_type   = dtype[["raster"]],
      updatevalue = ifelse(is.null(out_nodata), -Inf, out_nodata)
    )
  } else {
    raster::beginCluster()
    masked_out <- raster::clusterR(
      in_rast,
      fun       = function(x, y) {x*y},
      args      = list(y = raster::brick(temp_rastermask)),
      filename  = out_filename,
      options   = paste0("COMPRESS=", compress),
      overwrite = TRUE,
      data_type = dtype[["raster"]],
      NAflag    = ifelse(is.null(out_nodata), -Inf, out_nodata)
    )
    raster::endCluster()
  }

  #   __________________________________________________________________________
  #   clean up and return cropped image with format according to arguments  ####
  on.exit(unlink(temp_shapefile))
  on.exit(unlink(temp_rastermask))
  on.exit({if (exists("temprastfile")) unlink(temprastfile)})

  if (out_type == "rastobject") {
    out <- raster::brick(out_filename)
    names(out) <- paste0(rastinfo$bnames, "_masked")
    if (length(rastinfo$Z) != 0) {
      out <- raster::setZ(out, rastinfo$Z)
    }
    return(masked_out)
  } else {
    return(out_filename)
  }
}
