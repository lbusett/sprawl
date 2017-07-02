#' batch_clip
#'
#' @param in_folder     string input folder containing the raster files
#' @param out_folder    string output folder were clipped files are saved
#' @param in_clip_shape string input shape file used for clipping
#' @param recursive     logical if TRUE, analyze all subfolders of input_folder to find rasters
#' @param in_pattern    string pattern with which to search for rasters (default: "*.tif$)
#' @param in_nodata     numeric nodata value of input raster files
#' @param out_nodata    numeric nodata value of input raster files (defaults to NULL - if in_nodata is set and
#'  out_nodata is NULL, then out_nodata is reset to in_nodata)
#' @param out_format    string output format for clipped raster (defaults to "GTiff") - all valid gdal formats
#'  are allowed
#' @param out_proj      valid proj4 string (optional) output projection for clipped rasters. if not set,
#'  output rasters are in the same projection as the input rasters
#'
#' @export
#'
#' @importFrom raster extent dataType
#' @importFrom hash hash values
#' @importFrom gdalUtils gdalwarp
#' @importFrom sp CRS proj4string
#' @examples
#'

batch_clip = function (in_folder, out_folder, in_clip_shape, in_pattern = "*.tif$", in_nodata = NULL, out_nodata = NULL,
                          out_format = 'GTiff', out_proj = NULL, recursive = FALSE){

  # Identify files to be clipped
  if (recursive == FALSE) {
    in_files  <- list.files(in_folder, in_pattern)
    out_files <- file.path (out_folder, in_files)
  } else {
    in_files  <- list.files(in_folder,  in_pattern, recursive = TRUE)
    out_files <- file.path (out_folder, in_files)
  }

  # hash table to convert from rgdal to gdal formats
  dt_hash <- hash("INT1U" = "Byte",
                  "INT2S"="Int16",
                  "INT2U"="UInt16",
                  "INT4S"="Int32",
                  "INT4U"="UInt32",
                  "FLT4S"="Float32",
                  "FLT8S"="Float64")

  # cycle on files and do the clipping using gdal

  out_ext    <- extent(in_clip_shape)
  shape_proj <- proj4string(in_clip_shape)

  for(file in seq(along = in_files)) {

    in_file    <- file.path(in_folder, in_files[file])
    check_rast <- try(raster(in_file))

    if (class(check_rast) != "try-error") {

      out_file <- out_files[file]
      dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

#TODO - Set correctly raster projection
      #rast_proj <- proj4string(check_rast) # get input raster projection
      rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
# END TODO

      if (is.null(out_proj)) {out_proj <- rast_proj}
      in_data_type <- dataType(check_rast)
      out_dtype    <- as.character(hash::values(dt_hash, in_data_type))

      if (shape_proj != rast_proj) {   # If rast and shape proj differ, reproject the shape extent

        out_ext_rep <- extent(spTransform(SpatialPoints(
          data.frame(x = c(out_ext@xmin,out_ext@xmax), y = c(out_ext@ymax, out_ext@ymin)),
          proj4string = CRS(shape_proj)),rast_proj))
      } else {
        out_ext_rep <- out_ext
      }
      message("Clipping file: ", in_file)
      gdalwarp(in_file, out_file, s_srs = rast_proj, t_srs = out_proj, te = c(out_ext_rep@xmin, out_ext_rep@ymin,out_ext_rep@xmax, out_ext_rep@ymax),
               ot = out_dtype, tap = TRUE, r = 'near', tr = res(check_rast), srcnodata = in_nodata, dstnodata = out_nodata, of = out_format, overwrite = TRUE)
    } else {
      message("File: ", in_file, "is not in a recognized raster format - skipping it !")
    }
  }
}


