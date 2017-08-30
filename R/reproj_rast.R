#' @title reproj_rast
#' @description FUNCTION_DESCRIPTION
#' @param object PARAM_DESCRIPTION
#' @param out_proj PARAM_DESCRIPTION
#' @param out_extent PARAM_DESCRIPTION, Default: NULL
#' @param resamp_meth PARAM_DESCRIPTION, Default: 'near'
#' @param out_type PARAM_DESCRIPTION, Default: 'rastobject'
#' @param out_format PARAM_DESCRIPTION, Default: 'Gtiff'
#' @param compression PARAM_DESCRIPTION, Default: 'LZW'
#' @param out_filename PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  in_rast <- read_rast(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  out_proj <- "+init=epsg=3035"
#'  reproj_rast(in_rast, out_proj)
#'  }
#' }
#' @rdname reproj_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gdalUtils gdalwarp
reproj_rast <- function(object,
                        out_proj,
                        out_res      = NULL,
                        resamp_meth  = "near",
                        out_type     = "rastobject",
                        out_format   = "GTiff",
                        compression  = "LZW",
                        out_filename = NULL,
                        overwrite    = FALSE,
                        verbose      = TRUE,
                        ...) {

  message("reproj_rast --> Reprojecting `object` to ",
          get_proj4string(out_proj))

  temp_vrt <- tempfile(fileext = ".vrt")
  temp_vrt <- create_virtrast(object, temp_vrt)

  if (is.null(out_filename)) {
    out_filename <- tempfile( fileext = ifelse(out_format == "vrt",
                                               ".vrt",".tif"))
  }

  if (is.null(out_extent)) {
    te = ""
  } else {
    te = reproj_extent(out_extent, get_proj4string(object))
  }



  out <- gdalUtils::gdalwarp(
    srcfile = temp_vrt,
    dstfile = out_filename,
    s_srs   = get_proj4string(object),
    t_srs   = "+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    tr      = out_res,
    r       = resamp_meth,
    multi   = TRUE,
    to      = ifelse(out_format == "GTiff",
                     paste0("COMPRESS=", compression),
                     ""),
    overwrite = TRUE, verbose = T, output_Raster = T
  )

  if (out_type == "rastobject") {
    return(out)
  } else {
    return(out_filename)
  }
}
