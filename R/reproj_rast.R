#' @title Reproject a raster "R" object or file
#' @description Reproject a raster "R" object or file to a different reference
#'   system. The function is a simple wrapper around `gdalwarp` with
#'   additional checks on inputs allowing to specify the output projection
#'   in several ways:
#'     1: passing a valid proj4 string (e.g., `reproj_rast(in_rast, "+init=epsg:4326")`
#'     2: passing a numeric or character that can be interpreted as an
#'        EPSG code (e.g., `reproj_rast(in_vect, 4326)`);
#'     3: passing the name of a valid `R` vector or raster object:
#'        (e.g.,`reproj_rast(in_vect, rast_obj)`, with `rast_obj`
#'        being an existing `R` object;
#'     4: passing the path to a valid vector or raster file:
#'        EPSG code (e.g.,`reproj_rast(in_vect, "D:/Temp/myfile.tif")`
#'
#'   The reprojected raster is written to a temporary "GTiff" file within `R`
#'   tempdir to allow accessing it immediately from `R`, unless a specific output
#'   file name is provided with the `out_file` argument.
#' @param in_rast A `*Raster` object, or the path to a raster file
#' @param in_projobj `R` object or filename from which the output projection should
#'   be derived (see @description)
#' @param out_res `numeric (1 | 2)` desired resolution for the output in X and Y
#'   (in measure units of the OUTPUT projection). If a 1-element array is passed,
#'   the same  resolution is used for X and Y. If NULL, the output resolution is set
#'   automatically by gdalwarp based on input/output projections and the resolution
#'   of the input, Default: NULL
#' @param crop `logical` If TRUE, and `in_projobj` corresponds to a spatial object or
#'   filename, the output is also cropped on the extent of `in_projobj`,
#'  Default: FALSE
#' @param pix_buff `numeric` Dimension of a buffer around the extent to be
#'   added to it to guarantee that all the area of `in_projobj` is preserved
#'   in the reprojected dataset, Default: 1 (ignored if `crop == FALSE`)
#' @param resamp_meth `character ["near", "bilinear", "cubic", "cubicspline",
#' "lanczos", "average", "mode"]` Resampling method to be used by `gdalwarp` (See
#' http://www.gdal.org/gdalwarp.html), Default: 'near'
#' @param out_file `character` Path where the reprojected vector should be saved.
#'   __If NULL, the reprojected raster saved on `R` temporary folder, and will not
#'   be accessible after closing the session__, Default: NULL
#' @param out_type `character ["rastfile", "rastobject"]` If "rastfile", and `out_file`
#'   is not NULL, the function returns the name of the saved raster. Otherwise,
#'   it returns the reprojected raster as a `*Raster` object
#' @param out_format `character` Format to be used to save the reprojected raster,
#'  Default: 'GTiff'
#' @param compression `character ["NONE" | "PACKBITS" | "LZW" | "DEFLATE"]`
#'  Compression method to be used to save the  raster if `out_format` is "GTiff",
#'   Default: 'LZW'
#' @param warp_args Additional parameters to be passed to `gdalwarp`,
#'   Default: NULL (Not currently implemented)
#' @param overwrite `logical` If TRUE, overwrite existing files, Default: FALSE
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @param ... Other arguments (None currently implemented)
#' @return a `Raster` object, or the path of the file where the reprojected input
#'  was saved
#' @seealso http://www.gdal.org/gdalwarp.html
#' @examples
#' \dontrun{
#'  library(sprawl.data)
#'  # reproject a raster based on an output proj4string
#'  in_file <- system.file("extdata/OLI_test", "oli_multi_1000_b2.tif",
#'                          package = "sprawl.data")
#'  out_proj <- "+init=epsg:3035"
#'  reproj_rast(in_file, out_proj)
#'
#'  # reproject on projection of a different spatial object/file
#'  in_rast <- read_rast(in_file)
#'  my_vect <- get(load(system.file("extdata", "Lake.RData",
#'                          package = "sprawl.data")))
#'  out_rep <- reproj_rast(in_rast, my_vect)
#'  plot_rast_gg(out_rep, palette_name = "Greys", scalebar = FALSE,
#'   direction = -1) +
#'   geom_sf(data = my_vect, fill = "transparent", color = "red")
#'
#'  # reproject on projection of a different spatial object/file and crop on
#'  # its extent
#'
#'  out_cropped <- reproj_rast(in_rast, my_vect, crop = TRUE)
#'  plot_rast_gg(out_cropped, scalebar = F,  palette_name = "Greys",
#'   direction = -1) +
#'   geom_sf(data = my_vect, fill = "transparent", color = "red")
#' }
#' @rdname reproj_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom wrapr "%.>%"

reproj_rast <- function(in_rast,
                        in_projobj,
                        out_res         = NULL,
                        crop            = FALSE,
                        pix_buff        = 1,
                        resamp_meth     = "near",
                        out_type        = "rastobject",
                        out_format      = "GTiff",
                        compression     = "LZW",
                        out_file    = NULL,
                        warp_args       = NULL,
                        overwrite       = FALSE,
                        verbose         = TRUE,
                        ...) {

  . <- NULL
  call <- match.call()
  #   ____________________________________________________________________________
  #   Check arguments                                                         ####

  in_type <- get_rastype(in_rast)
  in_proj <- get_proj4string(in_rast)

  checkmate::assertSetEqual(
    (in_proj == "invalid"), FALSE)
    # info = glue::glue("reproj_rast --> Invalid projection detected for ",
    #                   call[[2]],". Aborting!"))

  out_proj <- get_proj4string(in_projobj)
  checkmate::assertSetEqual(
    out_proj == "invalid", FALSE)
    # info = glue::glue("reproj_rast --> Invalid projection detected in ",
    #                   call[[3]],". Aborting!"))

  checkmate::expect_choice(
    out_type, c("rastobject", "rastfile"),
    info = glue::glue("reproj_rast --> `out_type` must be \"rastobject\" or ",
                      "\"rastfile\". Aborting!")
  )

  if (verbose) {
    message("reproj_rast --> Reprojecting `", call[[2]], "` to ",
            get_proj4string(out_proj))
  }

  #   __________________________________________________________________________
  #   if input is a Raster object use create_virtrast to create a GDAL vrt  ####
  #   representing the object to be passed to gdalwarp. The vrt will inherit
  #   all "in_rast" characteristics (i.e., selected bands)

  if (in_type == "rastobject") {
    srcfile <- tempfile(fileext = ".vrt")
    srcfile <- create_virtrast(in_rast, srcfile, verbose = FALSE)
  } else {
    srcfile <- in_rast
  }

  #   __________________________________________________________________________
  #   If crop == TRUE, retrieve the extent from in_projobj if possible  ####
  #   The extent is then projected back to in_proj to gauarantee that all
  #   the area included in the bbox of in_projobj is included in the
  #   output raster
  if (crop & !is(in_projobj, "character")) {
    te = get_extent(in_projobj) %.>%
      reproj_extent(., out_proj = in_proj, enlarge = TRUE) %.>%
      (.@extent) %.>%
      (. + c(-pix_buff*res(in_rast)[1], -pix_buff*res(in_rast)[2],
             pix_buff*res(in_rast)[1], pix_buff*res(in_rast)[2]))
  }


  #   _________________________________________________________________________
  #   If no filename is specified, the reprojected raster                   ####
  #   is saved as a "tif" file in tempdir(), unless out_format is "vrt".
  #   In the latter case, the output is a reprojected virtual file (very fast)
  if (is.null(out_file)) {
    out_file <- tempfile(fileext = ifelse(out_format == "vrt",
                                              ".vrt",".tif"))
  }
  #   __________________________________________________________________________
  #   Build the gdalwarp call string                                        ####

  warp_string <- paste0(
    "-s_srs \"" , in_proj, "\"",
    " -t_srs \"", out_proj, "\"",
    if (crop) paste0(" -te ", paste(te, collapse = " "),""),
    if (crop) paste0(" -te_srs \"", in_proj, "\""),
    if (!is.null(out_res)) paste0(" -tr ", paste(out_res, collapse = " ")),
    " -r "    , resamp_meth,
    " -multi",
    " -of ", out_format,
    if (out_format == "GTiff") paste0(" -to COMPRESS=", compression, " "),
    if (overwrite) " -overwrite ",
    if (!is.null(warp_args)) warp_args,
    # "-srcnodata '-Inf' " ,
    # "-dstnodata '-Inf' ",
    " ",
    srcfile, " ",
    out_file
  )

  out <- system2(file.path(find_gdal(), "gdalwarp"), args = warp_string,
                 stdout = NULL)

  #   __________________________________________________________________________
  #   check the status of the call and issue messages on failure            ####

  if (out != 0) {
    stop("reproj_rast --> An error occurred while reprojecting `", call[[2]],
         "`.\nPerformed call to `gdalwarp` was: \n\"gdalwarp ", warp_string,
         "\"")
  }

  #   __________________________________________________________________________
  #   Return the result as filename or Raster object                        ####

  if (out_type == "rastobject") {
    out        <- read_rast(out_file, verbose = FALSE)
    rastinfo   <- get_rastinfo(in_rast, verbose = FALSE)
    names(out) <- rastinfo$bnames
    if (length(rastinfo$Z) != 0) {
      out <- setZ(out, rastinfo$Z)
    }
    return(out)
  } else {
    return(out_file)
  }
}
