#' @title reproj_rast
#' @description FUNCTION_DESCRIPTION
#' @param in_object PARAM_DESCRIPTION
#' @param outproj_object PARAM_DESCRIPTION
#' @param crop PARAM_DESCRIPTION, Default: NULL
#' @param pix_buff PARAM_DESCRIPTION, Default: NULL
#' @param resamp_meth PARAM_DESCRIPTION, Default: 'near'
#' @param out_type PARAM_DESCRIPTION, Default: 'rastobject'
#' @param out_format PARAM_DESCRIPTION, Default: 'Gtiff'
#' @param compression PARAM_DESCRIPTION, Default: 'LZW'
#' @param out_filename PARAM_DESCRIPTION, Default: NULL
#' @param warp_args PARAM_DESCRIPTION, Default: FALSE
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
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
reproj_rast <- function(in_object,
                        outproj_object,
                        out_res         = NULL,
                        crop            = FALSE,
                        pix_buff        = 5,
                        resamp_meth     = "near",
                        out_type        = "rastobject",
                        out_format      = "GTiff",
                        compression     = "LZW",
                        out_filename    = NULL,
                        warp_args       = NULL,
                        overwrite       = FALSE,
                        verbose         = TRUE,
                        ...) {

  call <- match.call()
  #   ____________________________________________________________________________
  #   Check arguments                                                         ####

  in_type <- get_spatype(in_object)
  assert_that(
    in_type %in% c("rastfile", "rastobject"),
    msg = paste0("reproj_rast --> `", call[[2]], " is not a valir `Raster*` ",
                 "object or raster filename. Aborting!")
  )


  in_proj <- get_proj4string(in_object, abort = FALSE)
  assert_that(
    in_proj != "invalid",
    msg = paste0("reproj_rast --> Unable to retrieve a valid proj4string from `",
                 call[[2]], "` . Aborting!")
  )

  out_proj <- get_proj4string(outproj_object, abort = FALSE)
  assert_that(
    out_proj != "invalid",
    msg = paste0("reproj_rast --> Unable to retrieve a valid proj4string from `",
                 call[[3]], "` . Aborting!")
  )

  assert_that(
    out_type %in% c("rastobject", "rastfile"),
    msg = paste0("reproj_rast --> `out_type must be \"rastobject\" or ",
                 "\"rastfile\". Aborting!")
  )

  if (verbose) {
    message("reproj_rast --> Reprojecting `", call[[2]], "` to ",
            get_proj4string(out_proj))
  }

  #   __________________________________________________________________________
  #   if input is a Raster object use create_virtrast to create a GDAL vrt  ####
  #   representing the object to be passed to gdalwarp. The vrt will inherit
  #   all "in_object" characteristics (i.e., bands)

  if (in_type == "rastobject") {
    srcfile <- tempfile(fileext = ".vrt")
    srcfile <- create_virtrast(in_object, srcfile, verbose = FALSE)
  } else {
    srcfile <- in_object
  }

  if (is.null(out_filename)) {
    out_filename <- tempfile(fileext = ifelse(out_format == "vrt",
                                              ".vrt",".tif"))
  }

  #   ____________________________________________________________________________
  #   If crop == TRUE, retrieve the extent from outproj_object if possible    ####
  #   The extent is then projected badk to in_proj to gauarantee that all
  #   the area included in the bbox of outproj_object is included in the
  #   output raster
  if (crop & !is(outproj_object, "character")) {
    te = get_extent(outproj_object) %.>%
       reproj_extent(., out_proj = in_proj, enlarge = TRUE) %.>%
       (.@extent) %.>%
       (. + c(-pix_buff*res(in_object)[1], -pix_buff*res(in_object)[2],
              pix_buff*res(in_object)[1], pix_buff*res(in_object)[2]))
  }


  #   __________________________________________________________________________
  #   Build the gdalwarp call string                                        ####

  warp_string <- paste(
    "-s_srs '" , in_proj, "'",
    " -t_srs '", out_proj, "'",
    if (crop) paste0(" -te ", paste(te, collapse = " "),""),
    if (crop) paste0(" -te_srs '", in_proj, "'"),
    if (!is.null(out_res)) paste0(" -tr ", paste(out_res, collapse = " ")),
    " -r "    , resamp_meth,
    " -multi",
    " -of ", out_format,
    if (out_format == "GTiff") paste0(" -to COMPRESS=", compression),
    if (overwrite) " -overwrite",
    if (!is.null(warp_args)) warp_args,
    srcfile, " ",
    out_filename
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
    out <- read_rast(out_filename, verbose = FALSE)
    rastinfo <- get_rastinfo(in_object, verbose = FALSE)
    names(out) <- rastinfo$bnames
    if (length(rastinfo$Z) != 0) {
      out <- setZ(out, rastinfo$Z)
    }
    return(out)
  } else {
    return(out_filename)
  }
}
