#' @title Retrieve useful info from a raster object or file
#' @description Function used to facilitate retrieval useful info from a raster object or
#'  file.
#' @param object either the name of a `Raster` object or a valid raster filename
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

get_rastinfo <- function(object) {
  call <- match.call()
  message("get_rastinfo --> Retrieving info from: `",
                     deparse(substitute(call)$object, "`"))
  object <- cast_rast(object, "rastobject")
  if (inherits(object, "RasterBrick")) {
    info <- list(nbands     = object@data@nlayers,
                 indbands   = seq(1, object@data@nlayers, 1),
                 ncols      = object@ncols,
                 nrows      = object@nrows,
                 ncells     = object@ncols * object@nrows,
                 res        = res(object),
                 bnames     = names(object),
                 fnames     = rep(object@file@name, object@data@nlayers),
                 Z          = object@z,
                 dtype      = object@file@datanotation,
                 proj4string = get_proj4string(object))
    return(info)
  }

  if (inherits(object, "RasterLayer")) {
    info <- list(nbands      = 1,
                 indbands    = object@data@band,
                 ncols       = object@ncols,
                 nrows       = object@nrows,
                 ncells      = object@ncols * object@nrows,
                 res         = res(object),
                 bnames      = names(object),
                 fnames      = object@file@name,
                 Z           = object@z,
                 dtype       = object@file@datanotation,
                 proj4string = get_proj4string(object))
    return(info)
  }
  if (inherits(object, "RasterStack")) {

    info <- list(nbands      = length(object@layers),
                 indbands    = unlist(lapply(object@layers,
                                             FUN = function(x) {x@data@band})),
                 ncols       = object@ncols,
                 nrows       = object@nrows,
                 ncells      = object@ncols * object@nrows,
                 res         = res(object),
                 bnames      = names(object),
                 fnames      = unlist(lapply(object@layers,
                                             FUN = function(x) {x@file@name})),
                 Z           = object@z,
                 dtype_rast  = unlist(lapply(object@layers,
                                             FUN = function(x) {x@file@datanotation})), #nolint
                 proj4string = get_proj4string(object))
    return(info)
  }
}
