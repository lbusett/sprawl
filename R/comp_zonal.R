#' @title comp_zonal
#' @description Function used to extract values of a single- or multi- band raster for pixels
#' corresponding to the features of a vector (Polygon, Point or Line)
#' @param in_rast input raster. Can be wither:
#' 1. A file name corresponding to a valid single- or multi- band raster (e.g. /my_folder/myfile.tif)
#' All `gdal` raster formats are supported
#' 2. An `raster`, `rasterStack` or `rasterBrick` object
#' @param in_vect_zones input vector object. Can be either:
#' 1. A `file name` corresponding to a valid ESRI shapefile (e.g. /my_folder/myshape.shp)
#' 2. An `R` `+sp` or `sf` object
#' @param selbands `2-element numeric array` defining starting and ending raster bands to be processed
#' (e.g., c(1, 10). Default: NULL, meaining that all bands will be processed
#' @param mask_object Not used for now - will allow to mask the raster before extraction of values, Default: NULL
#' @param rastres PARAM_DESCRIPTION, Default: NULL
#' @param id_field `character` (optional) field of the vector file to be used to identify the
#' zones from which data is to be extracted. MUST correspond to a univoc attribute (i.e., no replicated
#' values are allowed), If NULL (the default), zonal statistics are extracted on each row of the
#' shapefile, and a new column named `id_feat` is used on the output to distinguish the zones, Default: NULL
#' @param summ_data `logical` If TRUE, summary statistics of values belonging to each vector zone
#' are returned in `out$ts_summ~. Default statistics are average, median, standard deviation and variation
#' coefficient. Quantiles of the distribution can be also returned if `comp_quant == TRUE`,  Default: TRUE
#' @param full_data `logical` If TRUE, values of all pixels belonging to each vector zone are returned
#' in `out$ts_full', Default: TRUE
#' @param comp_quant `logical` if TRUE, also quantiles of the distributions of values are computed for each zone
#' and returned in  `out$ts_summ`, Default: FALSE
#' @param small `logical` if TRUE, values are returned also for small polygons not including any
#' pixel centroids, Default: TRUE
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param maxchunk PARAM_DESCRIPTION, Default: 5e+07
#' @param long PARAM_DESCRIPTION, Default: FALSE
#' @param addfeat PARAM_DESCRIPTION, Default: TRUE
#' @param addgeom PARAM_DESCRIPTION, Default: TRUE
#' @param keep_null PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param ncores PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @export
#' @details DETAILS
#' @examples
#' \dontrun{
#'   library(sprawl)
#'   in_rast  <- get(load(system.file("extdata", "in_rast.rda", package = "sprawl")))
#'   in_zones <- readshape(system.file("extdata","lc_polys.shp", package = "sprawl"), stringsAsFactors = T)
#'   stats    <- comp_zonal(in_rast, in_zones, long = FALSE, verbose = FALSE)
#'   options(tibble.width = Inf)
#'   stats
#'  }
#' @importFrom sf st_crs st_transform st_geometry st_as_sf
#' @importFrom sp proj4string
#' @importFrom dplyr mutate_if
#' @importFrom tibble as_tibble
#' @importFrom raster getZ
#' @importFrom magrittr %>%
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{lbusett@gmail.com}
#'
comp_zonal <- function(in_rast,
                       in_vect_zones,
                       selbands     = NULL,
                       mask_object  = NULL,
                       rastres      = NULL,
                       id_field     = NULL,
                       summ_data    = TRUE,
                       full_data    = TRUE,
                       comp_quant   = FALSE,
                       small        = TRUE,
                       na.rm        = TRUE,
                       maxchunk     = 50E6,
                       long         = FALSE,
                       addfeat      = TRUE,
                       addgeom      = TRUE,
                       keep_null    = FALSE,
                       verbose      = TRUE,
                       ncores       = NULL
)
{
  # create a list containing processing parameters (used to facilitate passing options to
  # accessory funcrtions)
  cz_opts <- list(selbands = selbands,   mask_object = mask_object,   rastres   = rastres,
                  id_field     = id_field,   summ_data   = summ_data, full_data = full_data,
                  comp_quant   = comp_quant, small       = small,     na.rm     = na.rm,
                  maxchunk     = maxchunk,   long        = long,      addfeat   = addfeat,
                  addgeom      = addgeom,    keep_null   = keep_null,   verbose   = verbose,
                  ncores       = ncores)

  #   ______________________________________________________________________________________________
  #   Check input types - send errors/warnings if not compliant + open the in_vect_zones if      ####
  #   or raster file if filenames were passed instead than a *sp/*sf object or *raster object  ####

  ras_type   <-  check_spatype(in_rast)
  zone_type  <-  check_spatype(in_vect_zones)
  if (!ras_type %in% "rastobject") {
    stop("Input in_rast is not a RasterStack or RasterBrick object")
  }

  if (zone_type == "none") {
    stop("Input in_vect_zones is not a valid vector/raster file or object !")
  }
  if (zone_type == "vectfile") {
    in_vect_zones <- readshape(in_vect_zones, stringsAsFactors = TRUE)
    zone_type   <- "sfobject"
  }

  if (zone_type == "rastfile") {
    in_vect_zones <- raster(in_vect_zones)
    zone_type   <- "rastobject"
  }

  # convert to an *sf objet if input is a *sp object
  if (zone_type == "spobject") {
    in_vect_zones <- as(in_vect_zones, "sf") %>%
      dplyr::mutate_if(is.character,as.factor) %>%
      tibble::as_tibble() %>%
      sf::st_as_sf()
    zone_type   <- "sfobject"
  }

  if (zone_type == "sfobject") {
    in_vect_zones <- in_vect_zones %>%
      dplyr::mutate_if(is.character,as.factor) %>%
      tibble::as_tibble() %>%
      sf::st_as_sf()
  }

  #   ____________________________________________________________________________
  ### check input arguments                                                   ####

  # if (!small_method %in% c("centroids", "full")) {
  #   warning("Unknown 'small_method' value - defaulting to 'centroids'")
  # }

  #   ____________________________________________________________________________
  #   Identify the bands/dates to be processed                                ####

  selbands    <- cz_getbands(in_rast, selbands, cz_opts$verbose)
  date_check  <- ifelse(attributes(selbands)$date_check, TRUE, FALSE )
  n_selbands  <- length(selbands)
  if (date_check) {
    dates <- raster::getZ(in_rast)
  } else {
    dates <- names(in_rast)
  }
  seldates <- dates[selbands]

  in_vect_zones$mdxtnq = seq(1:dim(in_vect_zones)[1])
  #   ____________________________________________________________________________
  #   Start cycling on dates/bands                                            ####

  if (n_selbands > 0) {

    #   ____________________________________________________________________________
    #   start processing for the case in which the in_vect_zones is a vector      ####

    if (zone_type == "sfobject") {

      # check if the id_field was passed and is correct. If not passed or not correct,
      # the record number is used as identifier in the output.
      #

      if (length(id_field) != 0) {
        if (!id_field %in% names(in_vect_zones)) {
          warning("Invalid 'id_field' value. Names of output columns (or values of 'feature' field if
                  `long` == TRUE) will be set to the record number of the shapefile feature")
          id_field <- NULL
        } else {
          if (length(unique(as.data.frame(in_vect_zones[,eval(id_field)])[,1])) != dim(in_vect_zones)[1]) {
            # warning("selected ID field is not univoc ! Names of output columns (or values of 'feature' field if
            #       `long` = TRUE) will be set to the record number of the shapefile feature")
            # id_field <- NULL
          }
        }
      }

      # check if the projection of the in_vect_zones and raster are the same - otherwise
      # reproject the in_vect_zones on raster CRS
      if (sf::st_crs(in_vect_zones)$proj4string != sp::proj4string(in_rast)) {
        in_vect_zones <- sf::st_transform(in_vect_zones, sp::proj4string(in_rast))
      }


      #   ____________________________________________________________________________
      #   Extract values if the zone pbject is a point shapefile                  ####
      #   TODO: extraction on LINES ! )

      if (inherits(sf::st_geometry(in_vect_zones), "sfc_POINT")) {
        # Convert the zone object to *Spatial to allow use of "raster::extract"
        out_list <- cz_points(in_vect_zones,
                              in_rast,
                              n_selbands,
                              selbands,
                              seldates,
                              id_field,
                              date_check,
                              long,
                              verbose,
                              addfeat,
                              addgeom,
                              keep_null)

        # end processing on points

        # TODO Implement processing for lines !!!!

      } else {
        #   __________________________________________________________________________________
        #   Extract values if the zone object is a polygon shapefile or already a raster  ####

        out_list <- cz_polygons(in_vect_zones, in_rast, seldates, selbands, n_selbands, date_check, cz_opts)
      }
    }
    return(out_list)
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
}



