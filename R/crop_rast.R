#' @title crop a raster file/rast_file on a given extent
#' @description function to crop a raster file/rast_file on a given extent. The extent
#'   can be:
#'  1. directly passed (as a `sprawlext` rast_file or numeric array);
#'  1. derived from a different rast_file (raster of vector file/rast_file) passed to
#'    the function (see details)
#' @param rast_object either an `R` object of class `Raster`, or a character string
#'   corresponding to a raster filename (with full path)
#' @param ext_object either an object of class `sprawlext`, or an `R` object or
#'   filename from which an object of class `sprawlext` can be obtained (see
#'    `sprawl::get_extent()`)
#' @param out_type `character` indicating to which type of rast_file the input should be
#'  re-cropped.
#'   1. "rastobject" return a `raster` rast_file accessing the saved cropped file
#'   2. "rastfile" return the filename of the GTiff file corresponding to the
#'     cropped GTiff file. If `out_filename == NULL`, this corresponds to a file
#'     saved in `R` temporary folder.
#'   3. "vrtfile" return the filename of the vrt file built for cropping
#'     the input raster (no saving to disk is performed)
#' , Default: "rastobject"
#' @param out_filename `character` filename to be used to save the cropped
#'   raster.
#' @param out_dtype `character` data type of the output masked files, according
#'   to gdal specifications for Gtiff files ("Byte", "UInt16", "Int16", "UInt32",
#'   "Int32", "Float32", "Float64", "CInt16", "CInt32", "CFloat32" and "CFloat64").
#'   If NULL, the data type is retrieved from the input, Default: NULL
#' @param compress `character` compression option to be used to saved the cropped
#'   raster ("None", "PACKBITS", "LZW", "DEFLATE), Default: "None"
#' @param verbose `logical` if FALSE, suppress processing messages, Default: TRUE
#' @return either a `Raster` object containing the cropped raster, or a gdal vrt
#'   or GTiff filename corresponding to it, depending on `out_type`.
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname crop_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom data.table last
#' @importFrom raster raster brick setZ

crop_rast <- function(rast_object,
                      ext_object,
                      out_type     = "rastobject",
                      out_filename = NULL,
                      out_dtype    = NULL,
                      compress     = "None",
                      verbose      = TRUE){

  call <- match.call()
  if (verbose) {
    message("crop_rast --> Cropping: ", call[[2]], " on extent of: ", call[[3]])
  }

  #   __________________________________________________________________________
  #   Create processing objects: rast_file is a filename, rast_object a     ####
  #   raster object used to retrieve extent, resolution, etc
  rast_file    <- cast_rast(rast_object, to = "rastfile")
  rast_object  <- cast_rast(rast_object, to = "rastobject")
  #   __________________________________________________________________________
  #   Retrieve extent info from `rast_object` and `ext_object`              ####

  rastinfo  <- get_rastinfo(rast_object, verbose = FALSE)
  rast_bbox <- get_extent(rast_object, abort = TRUE)
  crop_bbox <- get_extent(ext_object, abort = TRUE)
  dims      <- c(rastinfo$nrows, rastinfo$ncols)
  rbbox_ext <- rast_bbox@extent
  if (is.null(out_dtype)) {
    in_dtype <- rastinfo$dtype
    dtype    <- convert_rastdtype(in_dtype, "raster")
  } else {
    dtype    <- convert_rastdtype(out_dtype, "gdal")
  }

  #   __________________________________________________________________________
  #   reproject `ext_object` extent if necessary                            ####

  if (!(rast_bbox@proj4string == crop_bbox@proj4string)) {
    crop_bbox <- reproj_extent(crop_bbox, rast_bbox@proj4string)
  }
  cbbox_ext <- crop_bbox@extent

  #   __________________________________________________________________________
  #   retrieve xy coords of raster                                          ####

  col_coords <- rbbox_ext[1] + rastinfo$res[1] * (seq_len(dims[2]) - 1)
  row_coords <- rbbox_ext[2] + rastinfo$res[2] * (seq_len(dims[1]) - 1)

  #   __________________________________________________________________________
  #   compute a correct bounding box for the cropped raster, to avoid       ####
  #   moving around corners

  # xmin
  if (rbbox_ext[1] < cbbox_ext[1]) {
    which_lower    <- which(col_coords <= cbbox_ext[1])
    if (length(which_lower) != 0) {
      first_col    <- data.table::last(which_lower)
      rbbox_ext[1] <- col_coords[first_col]
    } else {
      first_col <- 1
    }
  }

  # ymin
  if (rbbox_ext[2] < cbbox_ext[2]) {
    which_lower    <- which(row_coords <= cbbox_ext[2])
    if (length(which_lower) != 0) {
      first_row    <- data.table::last(which_lower)
      rbbox_ext[2] <- row_coords[first_row]
    } else {
      first_row <- 1
    }
  }

  # xmax
  if (rbbox_ext[3] > cbbox_ext[3]) {
    which_lower  <- which(col_coords <= cbbox_ext[3])
    if (length(which_lower) != 0) {
    last_col     <- data.table::last(which_lower)
    rbbox_ext[3] <- col_coords[last_col]
    } else {
      last_col <- dims[2]
    }
  }

  # ymax
  if (rbbox_ext[4] > cbbox_ext[4]) {
    which_lower  <- which(row_coords <= cbbox_ext[4])
    if (length(which_lower) != 0) {
    last_row     <- data.table::last(which(row_coords <= cbbox_ext[4]))
    rbbox_ext[4] <- row_coords[last_row]
    } else {
      last_row <- dims[1]
    }
  }

  # TODO: Abort gracefully on incorrect rbbox_ext (e.g., because no intersection)

  # ____________________________________________________________________________
  # create a temporary vrt file corresponding to the band to be preocessed  ####
  # Using `create_virtrast~ allows flexibility in the case that a stack is
  # passed containing coming from different files

  temp_vrt <- tempfile(fileext = ".vrt")
  temp_vrt <- create_virtrast(rast_object,
                              out_vrt_file = temp_vrt,
                              out_extent   = rbbox_ext,
                              verbose      = FALSE)

  #   __________________________________________________________________________
  #   save/return the cropped raster according to arguments                 ####

  if (out_type == "vrtfile") {
    # Just return the vrt
    if (!is.null(out_filename)) {
      file.copy(temp_vrt, out_filename)
      temp_vrt <- out_filename
    }
    return(temp_vrt)

  } else {
    # Save and return filename or rastobject
    if (is.null(out_filename)) {
      out_filename <- tempfile(fileext = ".tif",
                               tmpdir = file.path(tempdir(), "sprawlcrop"))
    }
    make_folder(out_filename, type = "filename")
    translate_string <- paste("-of GTiff",
                              "-co", paste0("COMPRESS=", compress),
                              "-ot", dtype["gdal"][1,],
                              temp_vrt,
                              out_filename)

    system2(file.path(find_gdal(), "gdal_translate"),
            args = translate_string, stdout = NULL)

    if (out_type == "rastobject") {

      if (rastinfo$nbands == 1) {
        out <- raster::raster(out_filename)
      } else {

        out <- raster::brick(out_filename)
      }
      names(out) <- paste0(rastinfo$bnames)
      if (length(rastinfo$Z$time) == rastinfo$nbands) {
        out <- raster::setZ(out, rastinfo$Z$time)
      }
      return(out)
    } else {
      return(out_filename)
    }
  }
}
