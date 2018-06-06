#' @title gdal_polygonizeR
#' @description R Wrapper for the gdal_polygonize.py python script (http://www.gdal.org/gdal_polygonize.html)
#' This utility creates vector polygons for all connected regions of pixels in the raster sharing
#' a common pixel value. Each polygon is created with an attribute indicating the pixel value of
#' that polygon. Can be userful for example to create fishnet polygons (see "create_fishnet")
#'
#' @param x          `character`filename of a raster file, or "R" raster object
#' @param outshape   `character`filename of the desired output polygon shapefile
#' @param gdalformat  defaults to ESRI - don't change
#' @param pypath     `character` path of python  - if `NULL` (the default) the script tries to
#' automatically retrieve it
#' @param readpoly   `logical` If TRUE sends back the shapefile as a SpataialPolygonsDataFrame to R
#'     (defaults to FALSE)
#' @param quiet      `logical` if TRUE, limit the messages (defaults to TRUE)
#' @param overwrite  `logical` If TRUE overwrite shapefile if already existing (defaults to FALSE)
#'
#' @return NULL, or a SpataialPolygonsDataFrame (if readpoly = TRUE)
#' @export
#' @importFrom raster writeRaster
#' @importFrom methods is
#' @author Original code by Jonh Baumgartner
#' [https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/](), with
#' slight modifications by L.Busetto


gdal_polygonizeR <- function(x,
                             outshape   = NULL,
                             gdalformat = 'ESRI Shapefile',
                             pypath     = NULL,
                             readpoly   = TRUE,
                             quiet      = TRUE,
                             overwrite  = FALSE) {

  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }

  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
    if (any(f.exists)) {
      if (overwrite == FALSE) {
        stop(sprintf('File already exists: %s',
                     toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                    sep = '.')[f.exists])), call.=FALSE)
      } else (
        unlink(paste(outshape, c('shp', 'shx', 'dbf')))
      )
    }
    if (methods::is(x, 'Raster')) {

      raster::writeRaster(x, {f <- tempfile(fileext = '.tif')})
      rastpath <- normalizePath(f)
    } else if (is.character(x)) {
      rastpath <- normalizePath(x)
    } else stop('x must be a file path (character string), or a Raster object.')

    out <- system2('python', args = (paste0(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                                    pypath,
                                                    rastpath,
                                                    gdalformat,
                                                    outshape), " -fieldname id")),
                   stdout = TRUE)
    if (isTRUE(readpoly)) {

      out_file <- sprawl::change_fileext(outshape, "shp")
      shp <- sprawl::read_vect(out_file)
      return(shp)
    }
    return(NULL)
  }
}
