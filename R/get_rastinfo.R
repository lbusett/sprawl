#' @title Retrieve useful info from a raster object or file
#' @description Function used to facilitate retrieval useful info from a raster object or
#'  file.
#' @param in_rast a `Raster` object or the filename of a valid raster
#' @param stats `logical` If TRUE, retrieve also statisics (min, max, mean, etc.)
#'   for each band, Default: TRUE
#' @param quantiles `logical` If TRUE, retrieve also the quantiles, for each band,
#'   Default; FALSE
#' @param hist `logical` If TRUE, retrieve also the frequency histogram, for
#'   each band, Default: FALSE
#' @param verbose if FALSE suppress messages
#' @return `list` containing the following object:
#'   - nbands: `numeric` Number of bands of the object/file;
#'   - indbands: `numeric` If the object is associated with a file on disk, bands of the
#'     file to which each layer of the object correspond (see examples);
#'   - ncols: `numeric` Number of columns;
#'   - nrows: `numeric` Number of rows;
#'   - ncells: `numeric` Number of cells;
#'   - bnames: `character` Band names;
#'   - fnames: `character` If the object is associated with one or more files on disk,
#'     filename(s) corresponding to the different bands;
#'   - Z: `ANY` if the object has a "z" attribute set using raster::setZ(), values of the
#'     z attribute;
#'   - dtype: `character` datatype of the input raster (`raster` conventions)
#'   - proj4string: `character` proj4string of the object/file
#'   - units: `character` distance units of the projections
#' @examples
#' \dontrun{
#'  in_rast <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'   package = "sprawl.data")
#'  get_rastinfo(in_rast)
#'  in_rast <- system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",
#'   package = "sprawl.data")
#'  get_rastinfo(in_rast)
#'  }
#' @rdname get_rastinfo
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom glue glue

get_rastinfo <- function(in_rast,
                         stats     = TRUE,
                         quantiles = FALSE,
                         hist      = FALSE,
                         verbose   = TRUE) {
  #TODO Retrieve histogram and stats using get_raststats (if requested)
    call <- match.call()
  if (verbose) message("get_rastinfo --> Retrieving info from: `",
                     deparse(substitute(call)$in_rast), "`")

   in_rast <- cast_rast(in_rast, "rastobject")
  if (inherits(in_rast, "RasterBrick")) {
    info <- list(nbands      = in_rast@data@nlayers,
                 indbands    = seq(1, in_rast@data@nlayers, 1),
                 ncols       = in_rast@ncols,
                 nrows       = in_rast@nrows,
                 ncells      = in_rast@ncols * in_rast@nrows,
                 res         = raster::res(in_rast),
                 bnames      = names(in_rast),
                 fnames      = in_rast@file@name,
                 Z           = in_rast@z,
                 dtype       = in_rast@file@datanotation,
                 proj4string = get_proj4string(in_rast),
                 units       = get_projunits(get_proj4string(in_rast)))
    return(info)
  }

  if (inherits(in_rast, "RasterLayer")) {
    info <- list(nbands      = 1,
                 indbands    = in_rast@data@band,
                 ncols       = in_rast@ncols,
                 nrows       = in_rast@nrows,
                 ncells      = in_rast@ncols * in_rast@nrows,
                 res         = raster::res(in_rast),
                 bnames      = names(in_rast),
                 fnames      = in_rast@file@name,
                 Z           = in_rast@z,
                 dtype       = in_rast@file@datanotation,
                 proj4string = get_proj4string(in_rast),
                 units       = get_projunits(get_proj4string(in_rast)))
    return(info)
  }
  if (inherits(in_rast, "RasterStack")) {

    info <- list(nbands      = length(in_rast@layers),
                 indbands    = unlist(lapply(in_rast@layers,
                                             FUN = function(x) {x@data@band})),
                 ncols       = in_rast@ncols,
                 nrows       = in_rast@nrows,
                 ncells      = in_rast@ncols * in_rast@nrows,
                 res         = raster::res(in_rast),
                 bnames      = names(in_rast),
                 fnames      = unlist(lapply(in_rast@layers,
                                             FUN = function(x) {x@file@name})),
                 Z           = in_rast@z,
                 dtype       = unlist(lapply(in_rast@layers,
                                             FUN = function(x) {x@file@datanotation})), #nolint
                 proj4string = get_proj4string(in_rast),
                 units       = get_projunits(get_proj4string(in_rast)))
    return(info)
  }
  # #TODO Retrieve info forom GDALINFO
  #   if (inherits(object, "character")) {
  #
  #   info <- list(nbands      = length(object@layers),
  #                indbands    = unlist(lapply(object@layers,
  #                                            FUN = function(x) {x@data@band})),
  #                ncols       = object@ncols,
  #                nrows       = object@nrows,
  #                ncells      = object@ncols * object@nrows,
  #                res         = raster::res(object),
  #                bnames      = names(object),
  #                fnames      = unlist(lapply(object@layers,
  #                                            FUN = function(x) {x@file@name})),
  #                Z           = object@z,
  #                dtype       = unlist(lapply(object@layers,
  #                                            FUN = function(x) {x@file@datanotation})), #nolint
  #                proj4string = get_proj4string(object),
  #                units       = get_projunits(get_proj4string(object)))
  #   return(info)
  #   }

}
