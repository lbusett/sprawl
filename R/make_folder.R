#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION, Default: 'filename'
#' @param verbose PARAM_DESCRIPTION, Default: 'filename'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' path <- file.path(tempdir(), "test", "testfile.xyz")
#' dir.exists(dirname(path))
#' make_folder(path)
#' dir.exists(dirname(path))
#'
#' path <- file.path(tempdir(), "testdir")
#' dir.exists(path)
#' make_folder(path, type = "dirname")
#' dir.exists(dirname(path))
#' }
#' @rdname make_folder
#' @export
#' @importFrom assertthat assert_that
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
make_folder <- function(path, type = "filename", verbose = FALSE) {

  assertthat::assert_that(
    type %in% c("filename", "dirname"),
    msg = "make_folder --> `type` should be \"filename\" or \"dirname\". Aborting!" #nolint
  )

  if (verbose) {
    message("make_folder --> Creating ",
            ifelse(type == "filename", dirname(path), path),
            "folder")
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
  }
}
