#' @title change file extension
#' @description Helper function allowing to quckly change the extension of
#'  a file name
#' @param in_file `character` full path of a file
#' @param new_ext `character` new file extension (e.g., ".png")
#' @param full_path `logical` if TRUE, a full path with updated extension is
#'  returne, otherwise, only an updated basename is rerturned, Default: FALSE
#' @return `character` basename/full_path of `in_file`` with updated extension
#' @details DETAILS
#' @examples
#' in_file = "home/lb/tmp/prova.tif"
#' # return an updated basename
#' change_fileext(in_file, ".png")
#'
#' # return an updated full path
#' change_fileext(in_file, ".png", full_path = TRUE)
#'
#' @rdname change_fileext
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tools file_path_sans_ext

change_fileext <- function(in_file,
                           new_ext,
                           full_path = FALSE) {

  if (length(grep("^\\.",new_ext))==0) {
    new_ext <- paste0(".",new_ext)
  }

  if (full_path) {
    paste0(tools::file_path_sans_ext(in_file), new_ext)
  } else {
    paste0(basename(tools::file_path_sans_ext(in_file)), new_ext)
  }


}
