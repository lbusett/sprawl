#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param min_patch_area PARAM_DESCRIPTION
#' @param out_type PARAM_DESCRIPTION
#' @param out_file PARAM_DESCRIPTION
#' @param overwrite PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sieve_rast
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom sf st_geometry st_area st_polygon
sieve_rast <- function(in_rast,
                       min_patch_area,
                       out_type  = "rastobject",
                       out_file  = NULL,
                       overwrite = FALSE,
                       verbose   = TRUE)  {

  call <- as.list(match.call())
  if (verbose) (message("sieve_rast --> Sieving: ",
                        as.character(call[[2]]), " with minimum area of: ",
                        eval(call[[3]])))

  if (is.null(out_file)) {
    out_file <- tempfile(fileext = ".tif")
  }
  # poly_rast <- suppressMessages(rasterToPolygons(in_rast, dissolve = TRUE))%>%
  #   sprawl::cast_vect("sfobject") %>%
  #   sf::st_cast("POLYGON")

  tempshp <- tempfile(fileext = ".shp")
  poly_rast <- gdal_polygonizeR(in_rast,
                                tempshp,
                                overwrite = T)
  names(poly_rast)[1] <- "class"
  poly_rast$class <- as.factor(poly_rast$class)

  if(is.null(out_file)) {
    out_file <- tempfile(fileext = ".tif")
  }

  out_rast <- sieve_vect(poly_rast,
                            min_patch_area,
                            reassign_met = "aaa",
                            class_field = "class",
                            out_type = "sfobject") %>%
    mutate(class = as.numeric(class)) %>%
    sf::st_sf() %>%
    fasterize::fasterize(in_rast, field = "class") %>%
    writeRaster(filename = out_file, overwrite = TRUE)

  aaa <- fasterize::fasterize(bbb, in_rast, field = "class")

  if (out_type == "rastfile") {
    out_file
  } else {
    out_rast
  }
}
