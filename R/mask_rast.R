#' @title Mask a raster based on a vector
#' @description Masks a raster file or object on the basis of a vector file or object.
#'   Pixels not covered by the vector features are set to NoData. If the input raster
#'   is multi-band, the mask is automatically applied to all bands. An optional
#'   buffer can be applied to the input vector to allow a more "lenient" masking,
#'   or to remove also the borders of the vector.
#' @param in_rast Raster file or object inheriting class `raster` to be masked
#' @param mask 1. Vector file or object of class `*sf` or `sp` to be used as a mask
#'             2. Raster file or object of class `*sf` or `sp` to be used as a mask
#'  NOTE: If `mask` is a raster object or file, the function checks if it has the
#'  same number of pixels and extent of `in_rast`. If this is true, then `mask`
#'  is used directly to mask the input. Otherwise, it is first of all vectorized
#'  to a vector mask. The vector mask is then used in the processing.
#' @param mask_value `integer` if `mask` is a raster object/file, value corresponding
#'  to the areas __that should be removed__ from the ouptut, Default: 0 (_Ignored if
#'  the provided `mask` is a vector!_)
#' @param crop `logical` if TRUE, `in_rast` is also cropped on the extent of `mask`,
#'   Default: FALSE
#' @param buffer `numeric` if not NULL, width of a buffer to be applied to `mask`
#'   before masking `in_rast`. If negative, mask is "reduced" prior to masking
#'   (see examples), Default: NULL
#' @param out_nodata `numeric` value to be assigned to areas outside the mask,
#'   Default: NULL
#' @param out_type `character`
#'   - if == "rastobj", return a `Raster` object;
#'   - if == "filename", return the filename of the masked layer (GTiff or gdal vrt format
#'   depending on other arguments - see below)
#'   Default: "rastobj" (If an invalid string is provided, defaults to `rastobj`)
#' @param out_file `character` filename where the masked raster should be saved
#'   If NULL, the masked raster is saved on a temporary file in the `R` temporary
#'   folder, named `<basename(in_rast)>_sprawlmask.tif`. The file is saved in GTiff
#'   format, with `compress` compression.
#' @param out_dtype `character` data type of the output masked files, according
#'   to gdal specifications for GTiff files ("Byte", "UInt16", "Int16", "UInt32",
#'   "Int32", "Float32", "Float64", "CInt16", "CInt32", "CFloat32" and "CFloat64").
#'   If NULL, the data type is retrieved from the input, Default: NULL
#' @param compress `logical` allow verbose output from `foreach` for debugging
#'   purposes, Default: FALSE
#' @param overwrite `logical` if TRUE, and out_file is set and existing,
#'   existing files are overwritten, Default: FALSE
#' @param parallel `logical` if TRUE, use ClusterR to implement multicore
#'   processing. This speeds up execution for large rasters, Default: FALSE
#' @param cores `numeric` Number of cores to use in case of parallel processing.
#'   If not provided, it defaults to `parallel::detectCores()-2`
#' @param verbose `logical` if TRUE, extended processing information is sent to
#'  the console in the form of messages
#'
#' @return object of class `raster` (if out_type == `rastobj`), or `character`
#'   string corresponding to the filename of the created raster (if out_type ==
#'   `rastfile`)
#' @examples
#'
#' library(sprawl)
#' library(sprawl.data)
#' library(raster)
#' in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
#'                                   package = "sprawl.data"),
#'                        stringsAsFactors = TRUE)
#' in_rast  <- raster::stack(system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                        package = "sprawl.data"))[[1]]
#' in_polys <- sf::st_transform(in_polys, proj4string(in_rast))
#' masked   <- mask_rast(in_rast, in_polys, verbose = FALSE)
#' plot_rast(in_rast, in_poly = in_polys)
#' plot_rast(masked, in_poly = in_polys)
#'
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
#' @importFrom parallel detectCores

mask_rast <- function(in_rast,
                      mask,
                      mask_value   = 0,
                      crop         = FALSE,
                      buffer       = NULL,
                      out_type     = "rastobject",
                      out_file     = NULL,
                      out_dtype    = NULL,
                      out_nodata   = NULL,
                      compress     = "None",
                      overwrite    = FALSE,
                      parallel     = FALSE,
                      cores        = parallel::detectCores() - 2,
                      verbose      = TRUE) {

  # to avoid NOTE on check
  . <- NULL

  call <- as.list(match.call())
  if (verbose) (message("mask_rast --> Masking: ",
                        as.character(call[[2]]), " on: ",
                        as.character(call[[3]])))

  #   __________________________________________________________________________
  #   Check the arguments                                                   ####

  # checks on in_rast ----

  in_rast   <- cast_rast(in_rast, "rastobject")
  rastinfo  <- get_rastinfo(in_rast, stats = FALSE, verbose = FALSE)
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

  # check the type of `mask`
  mask_type <- get_spatype(mask)

  if (mask_type %in% c("rastfile", "rastobject")) {

    if (all(get_extent(mask)@extent == get_extent(in_rast)@extent) &
        all(raster::res(mask) == raster::res(in_rast))) {

      if (verbose) {message("We will use a raster mask")}
      temp_rastermask <- cast_rast(mask, "rastobject")
      if (!is.na(mask_value)) raster::NAvalue(temp_rastermask) <- mask_value
    } else {
      #TODO Implement an automatic vectorization instead !!!!!
      stop("raster mask has a diffewrent extent from `in_rast`. Aborting!")

    }

  }

  #   __________________________________________________________________________
  #   set the output folder (in tempdir if out_file == NULL)                ####
  if (is.null(out_file)) {
    # outfold      <- file.path(tempdir(), paste0("sprawlmask_",sample(1:1000))[1]) #nolint
    # out_file <- file.path(outfold, "sprawlmask.tif")
    out_file <- tempfile(fileext = ".tif")
  }

  make_folder(out_file, type = "filename")
  # In case of vector mask, create a temporary raster to be useed as mask ----

  if (!mask_type %in% c("rastfile", "rastobject")) {
    # checks on mask ----
    mask      <- cast_vect(mask, "sfobject")
    mask_proj <- get_proj4string(mask)

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
      in_rast <- crop_rast(in_rast,
                           mask,
                           out_type = "vrtfile") %>%
        raster::brick()
    }
    #   __________________________________________________________________________
    #   Rasterize the mask shapefile: allows great improvements in speed      ####
    #   on large raster. Use gdal_rasterize instaad than raster::rasterize to
    #   create the rasterized shapefile much faster and save as Byte

    if (verbose) {
      message("mask_rast --> Rasterizing the vector mask to a temporary TIFF ",
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

    temp_rastermask <- raster::raster(temp_rastermask)
    mask_value <- NA
  }
  #   __________________________________________________________________________
  #   Compute the mask - if parallel = TRUE, initialize a Cluster and use   ####
  #   raster::ClusterR(), otherwise use raster::overlay() !

  if (verbose) {
    message("mask_rast --> Masking bands")
  }

  if (!parallel) {

    masked_out <- raster::mask(
      in_rast,
      temp_rastermask,
      filename    = out_file,
      inverse     = FALSE,
      maskvalue   = NA,
      options     = paste0("COMPRESS=", compress),
      overwrite   = TRUE,
      datatype    = dtype[["raster"]][1],
      updatevalue = ifelse(is.null(out_nodata), NA, out_nodata)
    )
    dtype[["raster"]][1]
  } else {

    beginCluster(n = ifelse((cores > parallel::detectCores() - 1),
                            (parallel::detectCores() - 2),
                            cores))
    masked_out <- raster::clusterR(
      in_rast,
      fun       = function(x, y) {x*y},
      args      = list(y = raster::brick(temp_rastermask)),
      filename  = out_file,
      options   = paste0("COMPRESS=", compress),
      overwrite = TRUE,
      datatype  = dtype[["raster"]][1],
      NAflag    = ifelse(is.null(out_nodata), -Inf, out_nodata)
    )
    endCluster()

  }

  #   __________________________________________________________________________
  #   clean up and return cropped image with format according to arguments  ####
  on.exit(unlink(temp_shapefile))
  on.exit(unlink(temp_rastermask))
  on.exit({if (exists("temprastfile")) unlink(temprastfile)})

  if (out_type == "rastobject") {
    out <- raster::brick(out_file)
    names(out) <- paste0(rastinfo$bnames, "_masked")
    if (length(rastinfo$Z) != 0) {
      out <- raster::setZ(out, rastinfo$Z)
    }
    return(masked_out)
  } else {
    return(out_file)
  }
}
