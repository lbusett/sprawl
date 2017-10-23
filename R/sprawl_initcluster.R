#' @title initialize clustered processing on multiband raster
#' @description helper for initializing clustered processing on multiband raster
#' @param in_rast PARAM_DESCRIPTION
#' @param ncores PARAM_DESCRIPTION, Default: NULL
#' @param bands PARAM_DESCRIPTION, Default: NULL
#' @param maxchunk PARAM_DESCRIPTION, Default: 5e+05
#' @param out_on_console PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @details DETAILS
#' @examples
#'
#'  #EXAMPLE1
#'
#' @export
#' @rdname sprawl_initcluster
#' @importFrom doParallel registerDoParallel
#' @importFrom raster nlayers nrow ncol
#' @importFrom parallel detectCores makeCluster

sprawl_initcluster <- function(in_rast,
                               ncores         = NULL,
                               bands          = NULL,
                               maxchunk       = 50E5,
                               out_on_console = FALSE) {

  if (is.null(ncores)) {
    ncores <- parallel::detectCores() - 2
  }
  ncores <- min(c(ncores, (parallel::detectCores() - 2)), 8)
  if (is.null(bands)) {
    bands   <- seq(1, raster::nlayers(in_rast))
    if (length(bands) > 1) {
      n_bands <- length(bands)
    } else {
      n_bands <- 1
    }
  } else {
    bands   <- seq(bands[1], bands[2])
    n_bands <- length(bands)
  }

  if (n_bands < ncores) {
    ncores <- n_bands
  }

  if (out_on_console == TRUE) {
    clust <- parallel::makeCluster(ncores, outfile = "")
  } else {
    clust <- parallel::makeCluster(ncores)
  }

  doParallel::registerDoParallel(clust)

  nrows    <- raster::nrow(in_rast)
  ncols    <- raster::ncol(in_rast)
  n_cells  <- nrows * ncols
  n_chunks <- floor(n_cells / maxchunk) + 1
  # Initialize other variables and progress bar
  opts  <- list("ncores"    = ncores,
                "n_bands"   = n_bands,
                "bands"     = bands,
                "maxchunk"  = maxchunk/ncores,
                "nrows"     = nrows,
                "ncols"     = ncols,
                "n_cells"   = n_cells,
                "n_chunks"  = n_chunks)

  return(list(clust = clust, opts = opts))
}
