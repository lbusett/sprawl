#' @title crop a raster file/rast_file on a given extent
#' @description function to crop a raster file/rast_file on a given extent.
#'  The cropping extent can be:
#'  1. directly passed (as a `sprawlext` object or numeric array);
#'  2. derived from a different object from which a `sprawlext` object object
#'     can be derived (see details)
#' @param rast_object either an `R` object of class `Raster`, or a character string
#'   corresponding to a raster filename (with full path)
#' @param ext_object either an object of class `sprawlext`, or an `R` object or
#'   filename from which an object of class `sprawlext` can be obtained (see
#'    `sprawl::get_extent()`)
#' @param mask `logical` if TRUE, and `ext_object` is a polygon shapefile or
#'   `R` object, the output is also masked on the boundaries of `ext_object`
#' @param pad `numeric` extent of a "padding area" to be kept with respect to
#'   `crop_extent` (in number of pixels). Useful to be sure not to crop "too much"
#'  for example when cropping before extracting features suche as in `extract_rast`,
#'  Default: 1
#' @param out_type `character` indicates the type of object to be returned:
#'   1. "rastobject" return a `raster` rast_file accessing the saved cropped file
#'   2. "rastfile" return the filename of the GTiff file corresponding to the
#'     cropped GTiff file. If `out_file == NULL`, this corresponds to a file
#'     saved in `R` temporary folder.
#'   3. "vrtfile" return the filename of the vrt file built for cropping
#'     the input raster (**no saving to disk is performed**)
#' , Default: "rastobject"
#' @param out_file `character` filename to be used to save the cropped
#'   raster.
#' @param out_dtype `character` data type of the output masked files, according
#'   to gdal specifications for GTiff files ("Byte", "UInt16", "Int16", "UInt32",
#'   "Int32", "Float32", "Float64", "CInt16", "CInt32", "CFloat32" and "CFloat64").
#'   If NULL, the data type is retrieved from the input, Default: NULL
#' @param compress `character` compression option to be used to saved the cropped
#'   raster ("None", "PACKBITS", "LZW", "DEFLATE), Default: "None"
#' @param verbose `logical` if FALSE, suppress processing messages, Default: TRUE
#' @return either a `Raster` object containing the cropped raster, or a gdal vrt
#'   or GTiff filename corresponding to it, depending on `out_type`.
#' @examples
#'
#' in_file <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'                       package = "sprawl.data")
#' in_rast <- read_rast(in_file)
#'
#' in_vect     <- create_fishnet(in_rast, pix_for_cell = 60,
#'                               verbose = FALSE)[55:60,]
#'
#' plot_rast(in_rast[[1]], in_poly = in_vect)
#'
#' out_cropped <- crop_rast(in_rast, in_vect)
#'
#' get_extent(in_rast)
#' get_extent(out_cropped)
#'
#' plot_rast(out_cropped[[1]])
#'
#' @rdname crop_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom data.table last
#' @importFrom raster raster brick setZ
#' @importFrom sf st_intersects
crop_rast <- function(rast_object,
                      ext_object,
                      mask         = FALSE,
                      pad          = 1,
                      out_type     = "rastobject",
                      out_file = NULL,
                      out_dtype    = NULL,
                      compress     = "None",
                      verbose      = TRUE){

  call <- match.call()
  if (verbose) {
    message("crop_rast --> Cropping: ", call[[2]], " on extent of: ", call[[3]])
  }

  #   __________________________________________________________________________
  #   Create processing objects: rast_file is a filename, rast_object a     ####
  #   raster object used to retrieve extent, resolution, etc.
  rast_file    <- cast_rast(rast_object, to = "rastfile")
  rast_object  <- cast_rast(rast_object, to = "rastobject")
  #   __________________________________________________________________________
  #   Retrieve extent info from `rast_object` and `ext_object`              ####

  rastinfo  <- get_rastinfo(rast_object, verbose = FALSE)
  rast_bbox <- get_extent(rast_object, abort = TRUE)
  crop_bbox <- get_extent(ext_object, abort = TRUE)

  #   __________________________________________________________________________
  #   reproject `ext_object` extent if necessary                            ####

  if (!(rast_bbox@proj4string == crop_bbox@proj4string)) {
    crop_bbox <- reproj_extent(crop_bbox, rast_bbox@proj4string,
                               verbose = FALSE)
  }

  # Abort gracefully if not intersecting (TODO: Add test) ####
  test_intersect <- suppressMessages(
    sf::st_intersects(as(rast_bbox, "sfc_POLYGON"),
                      as(crop_bbox, "sfc_POLYGON"))[[1]]
  )
  if (length(test_intersect) == 0) {

    stop("crop_rast --> ", deparse(substitute(call)[[3]]),
         " does not intersect with ", deparse(substitute(call)[[2]]),
         ". Aborting!")

  }

  dims      <- c(rastinfo$nrows, rastinfo$ncols)
  rbbox_ext <- rast_bbox@extent
  cbbox_ext <- crop_bbox@extent

  #   __________________________________________________________________________
  #   retrieve xy coords of raster                                          ####

  col_coords <- rbbox_ext[1] + rastinfo$res[1] * (seq_len(dims[2] + 1) - 1)
  row_coords <- rbbox_ext[2] + rastinfo$res[2] * (seq_len(dims[1] + 1) - 1)

  #   __________________________________________________________________________
  #   compute a correct bounding box for the cropped raster, to avoid       ####
  #   moving around corners (add pad if requested and possible)

  # xmin
  if (rbbox_ext[1] < cbbox_ext[1]) {
    which_lower    <- which(col_coords < cbbox_ext[1])
    if (length(which_lower) != 0) {
      first_col    <- data.table::last(which_lower)
      if (first_col != 1) {
        first_col <- ifelse((first_col - pad) <= 1, 1, (first_col - pad))
      }
      rbbox_ext[1] <- col_coords[first_col + 1]
    }
  }

  # ymin
  if (rbbox_ext[2] < cbbox_ext[2]) {
    which_lower    <- which(row_coords < cbbox_ext[2])
    if (length(which_lower) != 0) {
      first_row    <- data.table::last(which_lower) + 1
      if (first_row != 1) {
        first_row <- ifelse((first_row - pad) <= 1, 1, (first_row - pad))
      }
      rbbox_ext[2] <- row_coords[first_row]
    }
  }

  # xmax
  if (rbbox_ext[3] > cbbox_ext[3]) {
    which_higher  <- which(col_coords > cbbox_ext[3])
    if (length(which_higher) != 0) {
      last_col     <- data.table::first(which_higher)
      if (last_col != dims[2]) {
        last_col <- ifelse((last_col + pad) >= dims[2], dims[2],
                           (last_col + pad))
      }
      rbbox_ext[3] <- col_coords[last_col]
    }
  }

  # ymax
  if (rbbox_ext[4] > cbbox_ext[4]) {
    which_higher  <- which(row_coords > cbbox_ext[4])
    if (length(which_higher) != 0) {
      last_row     <- data.table::first(which_higher)
      if (last_row != dims[1]) {
        last_row <- ifelse((last_row + pad) >= dims[1], dims[1],
                           (last_row + pad))
      }
      rbbox_ext[4] <- row_coords[last_row]
    }
  }

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
    if (!is.null(out_file)) {
      file.copy(temp_vrt, out_file)
      temp_vrt <- out_file
    }
    return(temp_vrt)

  } else {
    if (is.null(out_dtype)) {
      in_dtype <- rastinfo$dtype
      dtype    <- convert_rastdtype(in_dtype, "raster")
    } else {
      dtype    <- convert_rastdtype(out_dtype, "gdal")
    }

    # Save and return filename or rastobject
    if (is.null(out_file)) {
      out_file <- tempfile(fileext = ".tif",
                           tmpdir = file.path(tempdir(), "sprawlcrop"))
    }
    make_folder(out_file, type = "filename")


    translate_string <- paste("-of GTiff",
                              "-co", paste0("COMPRESS=", compress),
                              "-ot", dtype["gdal"][1,],
                              temp_vrt,
                              out_file)

    system2(file.path(find_gdal(), "gdal_translate"),
            args = translate_string, stdout = NULL)

    if (mask == TRUE) {

      ext_object <- cast_vect(ext_object, "sfobject")
      if (inherits(st_geometry(ext_object), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {

        if (!(rast_bbox@proj4string == crop_bbox@proj4string)) {
          ext_object <- sf::st_transform(ext_object, rast_bbox@proj4string,
                                         verbose = FALSE)
        }

        out_file <- mask_rast(out_file,
                              ext_object,
                              out_file = out_file,
                              out_type = "rastfile",
                              verbose = FALSE,
                              overwrite = TRUE)

      }

    }
    if (out_type == "rastobject") {

      if (rastinfo$nbands == 1) {
        out <- raster::raster(out_file)
      } else {

        out <- raster::brick(out_file)
      }
      names(out) <- paste0(rastinfo$bnames)
      if (length(rastinfo$Z$time) == rastinfo$nbands) {
        out <- raster::setZ(out, rastinfo$Z$time)
      }
      return(out)
    } else {
      return(out_file)
    }
  }
}
