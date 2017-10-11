#' @title migrate "virtual rasters" to a new location
#' @description function allowing to update the paths contained in "virtual rasters"
#'   such as GDAL VRTs or `R` rasterStacks "pointing" to files on disk in the case that
#'   the corresponding files on disk are moved.
#' @param in_obj `character` file path of an `RData` file corresponding to a
#'   `*Raster` object or a GDAL `vrt` file
#' @param new_path `character` path were the raster files to which `in_obj` points
#'   were moved.
#' @param out_file `character` filename were the updated `RData` or `vrt` file
#'   should be saved. If NULL, the new file is built by adding "_new" to the
#'   basename of the old one. If == "overwrite", the old file is overwritten.
#'   Default: NULL
#' @overwrite `logical` if TRUE, the old file will be overwritten (use with caution!),
#'   Default: FALSE
#' @return `character` path to the new "vrt" or "RData"
#' @examples
#' \dontrun{
#'  # suppose you had a gdal vrt file "pointing" to tiff files originally located in
#'  # "/home/mypath/myfolder", and you successively moved them to "/home/mypath/mynewfolder"
#'  # If you want to update the vrt file so that it keeps working:
#'
#'  #TODO (Remove the # after uploading a test dataset on sprawl.data)
#'  #old_vrt  <- "/home/mypath/myfolder/myvrt.vrt
#'  #new_path <- "/home/mypath/mynewfolder"
#'  #new_vrt  <- migrate_virtrast(old_vrt, new_path)
#'  #new_vrt
#'  #raster::stack(new_vrt)
#' }
#' @rdname migrate_virtrast
#' @export
#' @importFrom raster nlayers
#' @importFrom stringr str_split_fixed
#' @importFrom tools file_ext file_path_sans_ext
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

migrate_virtrast <- function(in_obj,
                             new_path,
                             out_file = NULL,
                             overwritr = FALSE) {
  UseMethod("migrate_virtrast")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @method migrate_virtrast default
#' @export
migrate_virtrast.default  <- function(in_obj,
                                      new_path,
                                      out_file = NULL) {
  call <- match.call()
  stop("migrate_virtrast --> ", call[[2]], " is not a `RData` or `GDAL vrt` ",
       "file. Aborting !")
}

#' @method migrate_virtrast character
#' @export

migrate_virtrast.character  <- function(in_obj,
                                        new_path  = NULL,
                                        out_file = NULL) {
  call <- match.call()

  if (!file.exists(in_obj)) stop("migrate_virtrast --> ", call[[2]], " does ",
                                  "not exist on your system. Aborting !")

  if (is.null(new_path)) {

    new_path <- tcltk::tk_choose.dir(
      default = "",
      caption = "Select folder containing the raster files"
    )
    if (is.na(new_path)) {
      stop("migrate_virtrast --> User selected to quit. Aborting !")
    }
  }
  #   __________________________________________________________________________
  #   If input file is a gdal vrt, substitute find the lines corresponding  ####
  #   to file paths and replace folder na,e with `new_path`

  if (tools::file_ext(in_obj) == "vrt") {
    file_in <- readLines(in_obj)
    for (line in (seq_along(file_in))) {
      line_i    <- file_in[line]
      is_source <- grep("SourceFilename", line_i )
      if (length(is_source) != 0) {
        old_file <- stringr::str_split_fixed(
          stringr::str_split_fixed(line_i, ">" ,2)[2], "<" ,2)[1]
        new_file <- file.path(new_path, basename(old_file))
        file_in[line] <- gsub(old_file, new_file, file_in[line])
      }
    }
    if (is.null(out_file)) {
      out_file <- paste0(tools::file_path_sans_ext(in_obj), "_new.vrt")
    } else {
      if (out_file == "overwrite") {
        out_file <- in_obj
      }
    }
    writeLines(file_in, out_file)
    return(out_file)
  } else {

    #   ________________________________________________________________________
    #   If input file is a RData file, open it as a rasterStack, then       ####
    #   substitute paths in the layer names

    if (tools::file_ext(in_obj) == "RData") {
      rrast_in <- try(get(load(in_obj)))
      if (!inherits(rrast_in, "Raster")) {
        stop("migrate_virtrast --> ", call[[2]], " does not appear to be ",
             "linked to a Raster object. Aborting !")
      } else {
        for (band in seq_len(raster::nlayers(rrast_in))) {
          old_file <- rrast_in[[band]]@file@name
          new_file <- file.path(new_path, basename(old_file))
          rrast_in[[band]]@file@name <- new_file
        }
        if (is.null(out_file)) {
          out_file <- paste0(tools::file_path_sans_ext(in_obj), "_new.RData")
        } else {
          if (out_file == "overwrite") {
            out_file <- in_obj
          }
        }
        save(rrast_in, file = out_file)
        return(out_file)
      }
    }
  }
}
