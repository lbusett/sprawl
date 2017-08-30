#' @title create a GDAL vrt file starting from a `Raster` object
#' @description create a GDAL vrt file starting from a `Raster` object
#' @param in_file PARAM_DESCRIPTION
#' @param out_vrt_file PARAM_DESCRIPTION
#' @param out_extent PARAM_DESCRIPTION, Default: NULL
#' @param rastinfo PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_virtrast
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

create_virtrast <- function(object,
                            out_vrt_file,
                            out_extent = NULL,
                            rastinfo = NULL) {


  rast_file <- cast_rast(object, "rastfile")
  if (is.null(rastinfo)) {
    rastinfo <- get_rastinfo(object)
  }

  if (is.null(out_extent)) out_extent <- get_extent(object)@extent

  if (rastinfo$nbands > 1) {

    if (length(unique(rastinfo$fnames)) > 1) {
      # buildvrt string on multi band rasters with bands coming from different
      # files: use the "-input-file-list" argument
      tmp_txt <- tempfile(fileext = ".txt")
      writeLines(rastinfo$fnames, tmp_txt)
      buildvrt_string <- paste("-te", paste(out_extent, collapse = " "),
                               paste(paste("-b ", rastinfo$indbands),
                                     collapse = " "),
                               "-separate",
                               "-input_file_list",
                               tmp_txt,
                               out_vrt_file)
    } else {
      # buildvrt string on multi band rasters with bands coming from the same
      # file
      buildvrt_string <- paste("-te ", paste(out_extent, collapse = " "),
                               paste(paste("-b ", rastinfo$indbands),
                                     collapse = " "),
                               out_vrt_file,
                               rast_file)
    }

  } else {
    # buildvrt string on single band rasters. The paste on the second line is
    # useful to get the correct band if the call is done on a subset of a stack
    # (e.g., rast_in[[3]] - see test_mask_rast_R)

    buildvrt_string <- paste("-te ", paste(out_extent, collapse = " "),
                             paste(paste("-b ", rastinfo$indbands),
                                   collapse = " "),
                             out_vrt_file,
                             rast_file)
  }

  system2(file.path(find_gdal(), "gdalbuildvrt"),
          args = buildvrt_string,
          stdout = NULL)
  return(out_vrt_file)

}
