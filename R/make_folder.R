#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION, Default: 'dirname'
#' @param verbose PARAM_DESCRIPTION, Default: 'FALSE'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'
#' path <- file.path(tempdir(), "testdir")
#' dir.exists(path)
#' make_folder(path)
#' dir.exists(dirname(path))
#'
#' path <- file.path(tempdir(), "test2", "testfile.xyz")
#' dir.exists(dirname(path))
#' make_folder(path, type = "filename")
#' dir.exists(dirname(path))
#'
#'
#' @rdname make_folder
#' @export
#' @importFrom assertthat assert_that
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
make_folder <- function(path, type = "dirname", verbose = FALSE) {

  assertthat::assert_that(
    type %in% c("filename", "dirname"),
    msg = "make_folder --> `type` should be \"filename\" or \"dirname\". Aborting!" #nolint
  )

  if (verbose) {
    message("make_folder --> Creating ",
            ifelse(type == "filename", dirname(path), path),
            " folder")
  }

  if (type == "filename") {
    path <- dirname(path)
  }
  if (!dir.exists(path)) {
    builddir <- try(dir.create(path, recursive = TRUE, showWarnings = FALSE))
    if (class(builddir) == "try-error") {
      stop("make_folder --> Unable to create the ", path, " folder.
           Please, check your inputs and verify file system permissions. ",
           "Aborting!")
    }
  } else {
    if (verbose) {
      message("make_folder --> specified `path` already exists!")
    }
  }
  invisible(path)
}
