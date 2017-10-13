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
#'   - **nbands**: `numeric` Number of bands of the object/file;
#'   - **indbands**: `numeric` If the object is associated with a file on disk, bands of the
#'     file to which each layer of the object correspond (see examples);
#'   - **ncols**: `numeric` Number of columns;
#'   - **nrows**: `numeric` Number of rows;
#'   - **ncells**: `numeric` Number of cells;
#'   - **bnames**: `character` Band names;
#'   - **fnames**: `character` If the object is associated with one or more files on disk,
#'     filename(s) corresponding to the different bands;
#'   - **Z**: `ANY` if the object has a "z" attribute set using raster::setZ(), values of the
#'     z attribute;
#'   - **dtype**: `character` datatype of the input raster (`raster` conventions)
#'   - **proj4string**: `character` proj4string of the object/file
#'   - **units**: `character` distance units of the projections
#'
#'   According to settings on `stats`, `quantiles` and `hist`, the following
#'   info are also reported:
#'
#'   - **stats**: `data.frame` containing min, max, average and standard deviation
#'     for each band (see examples);
#'   - **quants**: `data.frame` containing the quantiles of the distribution
#'     of raster values, for each band (100 value - 0.01 interval) (NULL is returned
#'     if `quants` == FALSE (the default));
#'   - **hists**: `data.frame` containing information about the distribtion of
#'     raster values for each band. the data frame includes the limits of each
#'     bin, the count of the number of pixels included in it, the corresponding
#'     frequency and the cumulated frequency at each bin  (NULL is returned
#'     if `hists` == FALSE (the default));
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
  call <- match.call()
  if (verbose) message("get_rastinfo --> Retrieving info from: `",
                       call[[2]], "`")

  rast_type <- get_rastype(in_rast)
  if (rast_type == "rastfile"){
    in_rast <- read_rast(in_rast)
  }

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

  }
  if (stats | quantiles | hist) {

    raststat <- get_raststats(in_rast,
                           hist = ifelse(hist, TRUE, FALSE),
                           quantiles = ifelse(quantiles, TRUE, FALSE))

    if (stats) info[["stats"]]         <- raststat$stats
    if (hist)  info[["hist"]]          <- raststat$hist
    if (quantiles) info[["quantiles"]] <- raststat$quantiles


  }
  return(info)
}
