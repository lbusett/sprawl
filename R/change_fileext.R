#' @title change file extension
#' @description Helper function allowing to quckly change the extension of
#'  a file name
#' @param in_file `character` full path of a file
#' @param new_ext `character` new file extension (e.g., ".png")
#' @param new_path `character` if not NULL the returned file name will use this
#'  new path instead than the one of the original file. If equal to "", only
#'  the updated basenmae is therefore returned (see examples), Default: NULL
#' @return `character` basename/full_path of `in_file`` with updated extension
#' @examples
#' in_file = "home/lb/tmp/prova.tif"
#' # return an updated full_path
#' change_fileext(in_file, ".png")
#'
#' #' update both path and extension
#' change_fileext(in_file, ".png", new_path = "/home/lb/new_path")
#'
#' # return an updated basename
#' change_fileext(in_file, ".png", new_path = "")
#'
#' @rdname change_fileext
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tools file_path_sans_ext

change_fileext <- function(in_file,
                           new_ext,
                           new_path = NULL) {

  if (length(grep("^\\.",new_ext))==0) {
    new_ext <- paste0(".",new_ext)
  }


  if (is.null(new_path)) {
    paste0(tools::file_path_sans_ext(in_file), new_ext)
  } else {
    if (new_path == "") {
      paste0(
        basename(tools::file_path_sans_ext(in_file)),
        new_ext)
    } else {
      file.path(new_path,
                paste0(
                  basename(tools::file_path_sans_ext(in_file)),
                  new_ext)
      )
    }
  }

}
