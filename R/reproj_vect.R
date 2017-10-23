#' @title Reproject a vector "R" object or file
#' @description Reproject a vector "R" object or file to a different reference
#'   system. The function is a simple wrapper around `sf::st_tranform` with
#'   additional checks on inputs allowing to specify the output projection
#'   in several ways:
#'     1: passing a valid proj4 string (e.g., `reproj_vect(in_vect, "+init=epsg:4326")`
#'     2: passing a numeric or character that can be interpreted as an
#'        EPSG code (e.g., `reproj_vect(in_vect, 4326)`);
#'     3: passing the name of a valid `R` vector or raster object:
#'        (e.g.,`reproj_vect(in_vect, rast_obj)`, with `rast_obj`
#'        being an existing `R` object;
#'     4: passing the path to a valid vector or raster file:
#'        EPSG code (e.g.,`reproj_vect(in_vect, "D:/Temp/myfile.tif")`
#'
#'   The reprojected vector can be automatically saved to a shapefile through
#'    `write_shape` by specifying a valid path with the `out_file` argument.
#' @param in_vect A vector object (`*sf` or `sp`), or the path to a vector file
#' @param in_projobj `R` object or filename from which the output projection should
#'   be derived (see @description )
#' @param out_file `character` Path where the reprojected vector should be saved.
#'   If NULL, the reprojected vector is not saved, but just sent back to the
#'   caller, Default: NULL
#' @param out_type `character ["vectfile" | "vectobject"]` If "vectfile", and `out_file`
#'   is not NULL, the function returns the name of the saved shapefile. Otherwise,
#'   it returns the reprojected vector object, with the format specified by
#'   out_class,  Default: 'vectobject'
#' @param out_class `character ["sf" | "sp"]` Specifies if the reprojected object
#'   should have class `sf` or `sp`. If NULL, the returned reprojected
#'   object has the same class of `in_vect`, Default: NULL
#' @param overwrite `logical` If TRUE, overwrite existing files, Default: FALSE
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return a vector of class `*sf` or `sp` (depending on `out_class`), or the
#'   path of the file where the reprojected input was saved (if out_type == "vectfile")
#' @seealso sf::st_write sf::st_transform write_shape
#' @examples
#'
#' library(sprawl.data)
#' library(gridExtra)
#' # reproject a vector file
#' in_vect <- system.file("extdata/shapes","lc_polys.shp",
#'                                    package = "sprawl.data")
#' in_vect <- read_vect(in_vect)
#' bounds  <- get_boundaries("PHL", 1)
#'
#' message("Input projection is: ", get_proj4string(in_vect))
#' plot_vect(in_vect, fill_var = "category", borders_layer = bounds)
#'
#' # reproject to 3857 (web mercator)
#' out_proj <-  3857
#' out_vect <- reproj_vect(in_vect, 3857)
#'
#' message("Output projection is: ", get_proj4string(out_vect))
#'
#' # Do the same, but also save the output to file
#' out_file <- tempfile(fileext = ".shp")
#' out_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
#' out_vect <- reproj_vect(in_vect, 3857, out_file = out_file)
#' read_vect(out_file)
#'
#' # use a different spatial file or object to set the output projection:
#' rast <- read_rast(system.file("extdata/MODIS_test", "EVIts_test.tif",
#'   package = "sprawl.data"))
#' out_vect <- reproj_vect(in_vect, rast)
#'
#' message("Output projection is: ", get_proj4string(out_vect))
#'
#' plot_vect(in_vect, borders_layer = bounds, fill_var = "category",
#'    title = "Original (lat/lon)")
#'
#' plot_vect(out_vect, borders_layer = bounds, fill_var = "category",
#'    title = "Reprojected (sinusoidal)")
#'
#'
#' @rdname reproj_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that is.writeable
#' @importFrom sf st_transform

reproj_vect <- function(in_vect,
                        in_projobj,
                        out_file  = NULL,
                        out_type  = "vectobject",
                        out_class = NULL,
                        overwrite = FALSE,
                        verbose   = TRUE) {
  call     <- match.call()

  # cast to sf on the fly
  in_sf <- cast_vect(in_vect, "sfobject")

  # If input of class *sp and out_class not defined, set out_class to "sp"

  if (is.null(out_class)){
    if (inherits(in_vect, "Spatial")) {
      out_class = "sp"
    } else {
      out_class = "sf"
    }
  }

  if (verbose) {

    if (is.character(in_projobj) | is.numeric(in_projobj)) {
      message("reproj_vect --> Reprojecting ", call[[2]],
              " to ", get_proj4string(eval(call[[3]])))
    } else {
      message("reproj_vect --> Reprojecting ", call[[2]],
              " to projection of", call[[3]])
    }
  }

  # reproject using sf::st_transform
  out      <- sf::st_transform(in_sf, get_proj4string(in_projobj))
  if (out_class == "sp") out <- as(out, "Spatial")

  # save to file if needed

  if (!is.null(out_file)) {
    if (!dir.exists(dirname(out_file))) {
      make_folder(out_file,
                  type = "filename",
                  verbose = FALSE)
    }
    assertthat::assert_that(
      assertthat::is.writeable(dirname(out_file)),
      msg = strwrap(paste0("reproj_vect --> ", call[[4]],
                           " is not a valid or writeable file. Aborting!"))
    )
    write_shape(out, out_file, overwrite = overwrite)

    if (out_type == "vectfile") return(out_file) else return(out)

  } else {
    return(out)
  }
}
