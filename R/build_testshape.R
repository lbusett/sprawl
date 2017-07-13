#' @title build_testshape
#' @description FUNCTION_DESCRIPTION
#' @param maxpolys PARAM_DESCRIPTION
#' @param rmax PARAM_DESCRIPTION, Default: 10
#' @param allow_overlaps PARAM_DESCRIPTION, Default: FALSE
#' @param to_file PARAM_DESCRIPTION, Default: FALSE
#' @param ext PARAM_DESCRIPTION, Default: NULL
#' @param crs PARAM_DESCRIPTION, Default: NULL
#' @param seed PARAM_DESCRIPTION, Default: 100
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{as_tibble}}

#'  \code{\link[stats]{runif}}
#' @rdname build_testshape
#' @export
#' @importFrom sf st_crs st_as_sf st_buffer st_set_crs st_agr st_union st_within st_intersection st_difference
#' @importFrom tibble as_tibble
#' @importFrom stats runif
#' @importFrom magrittr "%>%"
#'
build_testshape <- function(maxpolys,
                            rmax           = 10,
                            allow_overlaps = FALSE,
                            to_file        = FALSE,
                            ext            = NULL,
                            crs            = NULL,
                            seed           = 100) {

  if (is.null(ext) & is.null(crs)) {
    x_limits <- c(-180,180)
    y_limits <- c(-90,90)
    crs      <- 4326
  }

  if (!is.null(crs)) {
    test_crs <- try(sf::st_crs(crs))
    if (class(test_crs) == "try-error") {
      stop("build_testshape --> Unrecognized crs. Aborting.")
    }
  }

  if (!is.null(ext)) {
    if (is.numeric(ext) & length(ext) == 4)
      x_limits <- c(extent[1], extent[3])
    y_limits <- c(extent[2], extent[4])
  }

  xy <- data.frame(
    id = paste0("id_", 1:maxpolys),
    x = stats::runif(maxpolys, min(x_limits), max(x_limits)),
    y = stats::runif(maxpolys, min(y_limits), max(y_limits))) %>%
    tibble::as_tibble()
  polys <- sf::st_as_sf(xy, coords = c(2,3)) %>%
    sf::st_buffer(stats::runif(maxpolys, min = 2, max = rmax)) %>%
    sf::st_set_crs(4326)
  sf::st_agr(polys) <- "constant"
  if (!allow_overlaps) {
    for (i in 1:dim(polys)[1]) {
      cur_pol <- polys[i,]
      union <- sf::st_union(polys[-i,])
      if (sf::st_within(cur_pol, union)[[1]] == 1) {
        polys[i,] <- cur_pol
      } else {
        intersections <- sf::st_intersection(cur_pol, union)
        if (dim(intersections)[1] != 0) {
          union_intersects <- sf::st_union(intersections)
          polys[i,] <- sf::st_difference(cur_pol, union_intersects) #[1,1]
        }
      }
      # polys <- sf::st_make_valid(polys)
    }
  }
  #   int <- sf::st_intersection(polys, polys) %>%
  #     dplyr::filter(id != id.1) %>%
  #     sf::st_as_sf()
  #   if (length(int$id) > 0) polys <- sf::st_difference(x, sf::st_union(st_combine(y)))
  # }

  if (to_file) {
    tempfilename <- tempfile(fileext = ".shp")
    tempfile <- write_shape(polys, out_file = tempfilename)
    return(tempfilename)
  } else {
    return(polys)
  }
}
