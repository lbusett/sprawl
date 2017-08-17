#' @title crop a raster file/rast_file on a given extent
#' @description function to crop a raster file/rast_file on a given extent. The extent
#'   can be:
#'  1. directly passed (as a `sprawlext` rast_file or numeric array);
#'  1. derived from a different rast_file (raster of vector file/rast_file) passed to
#'    the function (see details)
#' @param rast_file either an `R` rast_file of class `Raster`, or a character string
#'   corresponding to a raster filename (with full path)
#' @param ext_rast_file either an rast_file of class `sprawlext`, or an `R` rast_file or
#'   filename from which an rast_file of class `sprawlext` can be obtained (see
#'    `sprawl::getextent()`)
#' @param out_type `character` indicating to which type of rast_file the input should be re-croped.
#'   1. "rastrast_file" return a `raster` rast_file accessing the saved cropped file
#'   2. "rastfile" return the filename of the GTiff file corresponding to the
#'     cropped GTiff file. If `out_filename == NULL`, this corresponds to a file
#'     saved in `R` temporary folder.
#'   3. "vrtfile" return the filename of the vrt file built for cropping
#'     the input raster (no saving to disk is performed)
#' , Default: "rastrast_file"
#' @param out_filename `character` filename to be used to save the cropped
#'   raster.
#' @param compress `character` compression option to be used to saved the cropped
#'   raster ("None", "PACKBITS", "LZW", "DEFLATE), Default: "None"
#' @param verbose `logical` if FALSE, suppress processing messages, Default: TRUE
#' @return
#' @examples
#' \dontrun{
#'  #EXAMPLE1
#'  }
#' @rdname crop_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

crop_rast <- function(object,
                      ext_object,
                      out_type     = "rastobject",
                      out_filename = NULL,
                      compress     = "None",
                      verbose      = TRUE){

  message("crop_rast --> Cropping: ", deparse(substitute(object)),
          " on extent of: ",
          deparse(substitute(ext_rast_file)))

  #   ____________________________________________________________________________
  #   Create processing objects: rast_file is a filename, rast_obj a raster   ####
  #   object used to retrieve extent, resolution, etc
  rast_file <- cast_rast(object, to = "rastfile")
  rast_obj  <- ifelse(inherits(object, "Raster"),
                      object,
                      raster::brick(rast_file))
  #   __________________________________________________________________________
  #   Retrieve extent info from `rast_obj` and `ext_object`                   ####

    bnames <- names(rast_obj)
    times  <- getZ(rast_obj)
    res    <- raster::res(rast_obj)
    rast_bbox <- get_extent(rast_obj, abort = TRUE)
    crop_bbox <- get_extent(ext_object, abort = TRUE)
    dims <- dim(rast_obj)


  rbbox_ext <- rast_bbox@extent
  cbbox_ext <- crop_bbox@extent

  #   __________________________________________________________________________
  #   reproject `ext_rast_file` if necessary                                   ####

  if (!(rast_bbox@projstring == crop_bbox@projstring)) {

    reproj_bbox <- sf::st_polygon(list(rbind(
      c(cbbox_ext["xmin"], cbbox_ext["ymin"]),
      c(cbbox_ext["xmin"], cbbox_ext["ymaxn"]),
      c(cbbox_ext["xmax"], cbbox_ext["ymax"]),
      c(cbbox_ext["xmin"], cbbox_ext["ymin"])))
    ) %>%
      sf::st_sfc(crs = crop_bbox@projstring) %>%
      sf::st_transform(rast_bbox@projstring) %>%
      get_extent()
  }

  #   __________________________________________________________________________
  #   retrieve xy coords of raster                                          ####

  col_coords <- rbbox_ext[1] + res[1] * seq_len(dims[2])
  row_coords <- rbbox_ext[2] + res[2] * seq_len(dims[1])

  #   __________________________________________________________________________
  #   compute a correct bounding box for the cropped raster, to avoid       ####
  #   moving around corners

  # xmin
  if (rbbox_ext[1] < cbbox_ext[1]) {
    first_col    <- data.table::last(which(col_coords <= cbbox_ext[1]))
    rbbox_ext[1] <- col_coords[first_col - 1]
  }

  # ymin
  if (rbbox_ext[2] < cbbox_ext[2]) {
    first_row    <- data.table::last(which(row_coords <= cbbox_ext[2]))
    rbbox_ext[2] <- row_coords[first_row - 1]
  }

  # xmax
  if (rbbox_ext[3] > cbbox_ext[3]) {
    last_col     <- data.table::last(which(col_coords <= cbbox_ext[3]))
    rbbox_ext[3] <- col_coords[last_col + 1]
  }

  # ymax
  if (rbbox_ext[4] > cbbox_ext[4]) {
    last_row     <- data.table::last(which(row_coords <= cbbox_ext[4]))
    rbbox_ext[4] <- row_coords[last_row + 1]
  }

  # ____________________________________________________________________________
  # create a temporary vrt file corresponding to the band to be preocessed  ####
  # This allows flexibility in the case that a stack is passed containing
  # coming from different files
  #
  # If the bands in the original stack are not all coming
  # from the same on-disk raster, build a txt file to tell gdalbuildvrt
  # which band comes from where !

  temp_vrt <- tempfile(fileext = ".vrt")

  if (raster::nlayers(rast_obj) > 1) {

    bands <- list()
    files <- list()

    for (bb in seq_len(raster::nlayers(rast_obj))) {

      bands[[bb]] <- rast_obj[[bb]]@data@band
      files[[bb]] <- rast_obj[[bb]]@file@name

    }
    bands <- unlist(bands)
    files <- unlist(files)

    if (length(unique(files)) > 1) {

      tmp_txt <- tempfile(fileext = ".txt")
      writeLines(files, tmp_txt)
      buildvrt_string <- paste("-te", paste(rbbox_ext, collapse = " "),
                               "-separate",
                               "-input_file_list",
                               tmp_txt,
                               temp_vrt)
    } else {

      buildvrt_string <- paste("-te ", paste(rbbox_ext, collapse = " "),
                               paste(paste("-b ", bands), collapse = " "),
                               temp_vrt,
                               rast_file)
    }

  } else {

    buildvrt_string <- paste("-te ",
                             paste(rbbox_ext, collapse = " "),
                             temp_vrt,
                             rast_file)
  }

  system2(file.path(find_gdal(), "gdalbuildvrt"),
          args = buildvrt_string,
          stdout = NULL)

  #   ____________________________________________________________________________
  #   save/return the cropped raster according to arguments                   ####

  if (out_type == "vrtfile") {
    return(temp_vrt)
  } else {

    if (is.null(out_filename)) {
      outdir = file.path(tempdir(), "sprawlcrop")
      out_filename = tempfile(fileext = ".tif")
    } else {
      outdir = dirname(out_filename)
    }
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    translate_string <- paste("-of GTiff",
                              "-co", paste0("COMPRESS=", compress),
                              temp_vrt,
                              out_filename)

    system2(file.path(find_gdal(), "gdal_translate"),
            args = translate_string, stdout = NULL)

    if (out_type == "rastobject") {
      out <- raster::brick(out_filename)

      names(out) <- paste0(bnames, "_cropped")
      if (!is.null(times)) {
        out <- setZ(out, times)
      }
      return(out)
    } else {
      return(out_filename)
    }
  }
}
