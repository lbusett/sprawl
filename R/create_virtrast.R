#' @title create a GDAL vrt file starting from a `Raster` object
#' @description creates a GDAL vrt file starting from a `Raster` object or
#'  file name.
#' @param in_rast a `*Raster` object or the path to a valid raster file
#' @param out_vrt_file `character` path were the vrt file must be saved. If NULL,
#'   the file is saved in "R" temporary folder , Default: NULL
#' @param out_extent `numeric (4)` coordinates of a bounding box (xmin, ymin,
#'   xmax, ymax). If provided, the created vrt file will correspond
#'   to a "cropped" version of `in_rast`. If NULL, no cropping will be done,
#'   Default: NULL
#' @param verbose If FALSE, processing messages are suppressed, Default: TRUE
#' @return `character` path to the created vrt file
#' @export
#' @examples
#' library(sprawl.data)
#' in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                      package = "sprawl.data")
#' in_rast <- read_rast(in_file, bands = 5)
#' vrt     <- create_virtrast(in_rast)
#' vrt
#'
#' # reading in the vrt gives a raster object
#' rast_from_vrt <- read_rast(vrt)
#' rast_from_vrt
#'
#' @rdname create_virtrast
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

create_virtrast <- function(in_rast,
                            out_vrt_file = NULL,
                            out_extent   = NULL,
                            verbose      = TRUE) {

  call <- match.call()
  if (verbose) {
    message("create_virtrast --> Creating virtual file from ", call[[2]])
  }

  if (get_rastype(in_rast) == "rastobject") {
    rast_file <- cast_rast(in_rast, "rastfile")
  }

  rastinfo <- get_rastinfo(in_rast, stats = FALSE, verbose = FALSE)
  if (any(rastinfo$fnames == "")) {
    rastinfo <- get_rastinfo(rast_file, stats = FALSE, verbose = FALSE)
  }

  if (is.null(out_vrt_file)) {
    out_vrt_file <- tempfile(fileext = ".vrt")
  }

  if (rastinfo$nbands > 1) {

    if (length(unique(rastinfo$fnames)) > 1) {
      # buildvrt string on multi band rasters with bands coming from
      # different  files (i.e., rasterStack: use the "-input-file-list" argument
      # with "-separate"
      tmp_txt <- tempfile(fileext = ".txt")
      writeLines(rastinfo$fnames, tmp_txt)
      buildvrt_string <- paste(
        if (!is.null(out_extent)) paste0("-te ",
                                         paste(out_extent, collapse = " ")),
        paste(paste("-b ", rastinfo$indbands),
              collapse = " "),
        "-separate",
        "-input_file_list",
        tmp_txt,
        out_vrt_file)
    } else {
      # buildvrt string on multi band rasters with bands coming from the same
      # file (i.e., rasterBrick): use just -b with no "-separate
      buildvrt_string <- paste(if (!is.null(out_extent)) paste0("-te ",
                                                                paste(out_extent, collapse = " ")),
                               paste(paste("-b ", rastinfo$indbands),
                                     collapse = " "),
                               out_vrt_file,
                               rast_file[1])
    }

  } else {
    # buildvrt string on single band rasters. The paste on the second line is
    # useful to get the correct band if the call is done on a subset of a stack
    # (e.g., rast_in[[3]] - see test_mask_rast_R)

    buildvrt_string <- paste(
      if (!is.null(out_extent)) paste0("-te ", paste(out_extent, collapse = " ")),
      paste(paste("-b ", rastinfo$indbands),
            collapse = " "),
      out_vrt_file,
      rast_file)
  }

  vrt_build <- suppressWarnings(try(system2(file.path(find_gdal(), "gdalbuildvrt"),
                                            args = buildvrt_string,
                                            stdout = NULL,
                                            stderr = TRUE)))

  if (!is.null(attr(vrt_build, "status"))) {

    stop("creat_virtrast --> An error occurred while creating the vrt file.",
         " The call to gdalbuildvrt was: gdalbuildvrt ", deparse(substitute(buildvrt_string)))

  }

  return(out_vrt_file)

}
