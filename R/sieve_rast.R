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

  tempshp <- tempfile(fileext = ".shp")
  poly_rast <- gdal_polygonizeR(in_rast,
                                tempshp,
                                overwrite = T)
  names(poly_rast)[1] <- "class"
  poly_rast$class <- as.factor(poly_rast$class)

  if(is.null(out_file)) {
    out_file <- tempfile(fileext = ".tif")
  }

  out_sf <- sieve_vect(poly_rast,
                       min_patch_area,
                       reassign_met = "aaa",
                       class_field = "class",
                       out_type = "sfobject") %>%
     mutate(class = as.numeric(class)) %>%
     sf::st_sf() %>%
     sf::st_cast("MULTIPOLYGON")

  if (out_type %in% c("rastfile", "rastobject")) {
    out_rast <- fasterize::fasterize(in_rast, field = "class") %>%
      writeRaster(filename = out_file, overwrite = TRUE,
                  datatype = "INT1U")
    ifelse(out_type == "rastfile", return(out_file), return(rastobject))
  } else {
    ifelse(out_type == "sfobject", return(out_sf))

  }

  if (out_type == "sfobject") {
    out_sf
  } else {
    tempshp
  }

}
