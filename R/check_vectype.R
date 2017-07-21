#' @title check if an object is a `R` vector or vector file
#' @description accessory function to check if an object passed to the function corresponds to
#' a `*sp` Object, a `sf` object, or a file corresponding to a valid ogr vector,
#' @param in_vect PARAM_DESCRIPTION
#' @param abort PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gdalUtils]{ogrinfo}}
#' @rdname check_vectype
#' @export
#' @importFrom gdalUtils ogrinfo
#'
check_vectype <- function(in_vect,
                          abort = FALSE) {

  vec_type <-  "none"

  # Is in_vect a sp object ? ----
  if (inherits(in_vect,"Spatial")) {
    vec_type <- "spobject"
  } else {
    # Is in_vect a sf object ? ----
    if (inherits(in_vect,"sf") | inherits(in_vect,"sfc")) {
      vec_type <- "sfobject"
    } else {
      # Is in_vect a valid ogr vector file ? ----
      if (class(in_vect) == "character" & file.exists(in_vect)) {
        vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(in_vect, al = TRUE, so = TRUE), silent = TRUE))
        if (is.null(attr(vecttry, "status"))) {
          vec_type <- "vectfile"
        }
      }
    }
  }
  # aborting if vec_type == "none" and abort = TRUE ----
  if (vec_type == "none" & abort == TRUE) {
    stop("check_vectype --> ", in_vect, " is not a vector (`*sf`` or `*sp` object or recognized
             vector file. Aborting !")
  }
  return(vec_type)
}
