#' @title extract raster values on features of a vector
#' @description Function used to extract values of a single- or multi- band raster
#'   for pixels corresponding to the features of a vector (Polygon, Point or Line)
#' @param in_rast input raster. Can be either:
#'   1. A file name corresponding to a valid single- or multi- band raster (e.g. /my_folder/myfile.tif)
#'   All `gdal` raster formats are supported
#'   2. An `raster`, `rasterStack` or `rasterBrick` object
#' @param in_vect input vector object containing zones from which data has to be
#'   extracted.
#'   Can be either:
#'   1. A `file name` corresponding to a valid ESRI shapefile (e.g. /my_folder/myshape.shp)
#'   2. An `R` `+sp` or `sf` object
#' @param rast_type `character` ("continuous" | "categorical) specifies if the
#'   values passed in `in_data` represent a continuous or categorical variable.
#'  (see @description)
#' @param selbands `2-element numeric array` defining starting and ending raster
#'   bands to be processed (e.g., c(1, 10). Default: NULL, meaning that all bands
#'   will be processed
#' @param rastres `numeric(2)`, if not null, the input raster is resampled to `rastres`
#'   prior to data extraction using nearest neighbour resampling. This is useful
#'   in case the polygons are small with respect to `in_rast` resolution,
#'   Default: NULL (meaning no resampling is done)
#' @param id_field `character` (optional) field of the vector file to be used to
#'   identify the zones from which data is to be extracted, If NULL (or invalid)
#'   zonal statistics are extracted on each row of the shapefile, and a new column
#'   named `id_feat` is used on the output to distinguish the zones, Default: NULL
#' @param summ_data `logical` If TRUE, summary statistics of values belonging to
#'   each vector zone are returned in `out$stats`. Default statistics are average,
#'   median, standard deviation and variation coefficient. Quantiles of the
#'   distribution can be also returned if `comp_quant == TRUE`,  Default: TRUE
#' @param full_data `logical` If TRUE, values of all pixels belonging to each vector
#'   zone are returned in `out$alldata` (Setting this to FALSE can be a good
#'   idea for large datasets), Default: TRUE
#' @param comp_quant `logical` if TRUE, also quantiles of the distributions of
#'   values are computed for each zone and returned in  `out$ts_summ`,
#'   Default: FALSE (Ignored if rast_type == "categorical")
#' @param comp_freq `logical` if TRUE, also frequencies of the the different classes
#'   of a categorical raster present in each polygon are returned, in `out$ts_summ`
#'   Default = FALSE (Ignored if rast_type == "continuous")
#' @param long_format `logical` if TRUE, extraction **on points** provides data
#'   in long format (i.e., only one "value" column, with an "id" column)
#'   allowing to select specific points, Default: TRUE
#' @param FUN `character` or function name, Default: NULL
#' @param small `logical` if TRUE, values are returned also for small polygons not
#'   including any pixel centroids. Values are taken from all cells "touched" by
#'   the small polygon, Default: TRUE
#' @param na.rm `logical` If TRUE, NA values are removed while computing statistics,
#'   Default: TRUE
#' @param maxchunk Maximum chunk size (provisional), Default: 5e+06
#' @param join_feat_tbl `logical` If TRUE, columns of the attribute table of the
#'  `in_vect` layer are joined to results of the computation, Default: TRUE
#' @param join_geom `logical`, If TRUE, the output sent out as an `sf` object,
#'   preserving the geometry of `in_vect`. Note that this leads to duplication
#'   of geometries, and may be very slow for large datasets. Default: FALSE
#' @param keep_null `logical` If TRUE, the output preserves features of `in_vect`
#'   falling outside the extent of `in_rast`. Values for these features are set
#'   to NA, Default: FALSE
#' @param verbose `logical` If TRUE, messages concerning the processing status
#'   are shown in the console, Default: TRUE
#' @param ncores `numeric` maximum number of cores to be used in the processing.
#'   If NULL, defaults to  available cores - 2, but up to a maximum of 8. If
#'   user-provided is greater than available cores - 2  or greater than 8,
#'   ncores is re-set to the minimum between those two.
#' @return out_list `list` containing two tibbles: `out_list$summ_data` contains
#'   summary statistics, while `out_list$alldata` contains the data of all pixels
#'   extracted (see examples).
#' @export
#' @examples
#'
#' library(sprawl)
#' library(sprawl.data)
#' library(raster)
#' library(tibble)
#' in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
#'                       package = "sprawl.data"), stringsAsFactors = T)
#' in_rast  <- raster::stack(system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                           package = "sprawl.data"))
#' in_rast  <- raster::setZ(in_rast, doytodate(seq(1,366, by = 8), year = 2013))
#'
#' out      <- extract_rast(in_rast, in_polys, verbose = FALSE)
#'
#' # Statistics for the different polygons
#'
#' as_tibble(out$stats)
#'
#' # Data from all pixels, grouped by polygon
#'
#' as_tibble(out$alldata)
#'
#'
#' @importFrom sf st_crs st_transform st_geometry st_as_sf
#' @importFrom sp proj4string
#' @importFrom dplyr mutate_if
#' @importFrom tibble as_tibble
#' @importFrom raster getZ
#' @importFrom magrittr %>%
#' @rdname extract_rast
#' @author Lorenzo Busetto, phD (2017) \email{lbusett@gmail.com}
#'
extract_rast <- function(in_rast,
                         in_vect,
                         rast_type     = "continuous",
                         selbands      = NULL,
                         rastres       = NULL,
                         id_field      = NULL,
                         summ_data     = TRUE,
                         full_data     = TRUE,
                         comp_quant    = FALSE,
                         comp_freq     = FALSE,
                         long_format   = TRUE,
                         FUN           = NULL,
                         small         = TRUE,
                         na.rm         = TRUE,
                         maxchunk      = 50E6,
                         join_feat_tbl = TRUE,
                         join_geom     = TRUE,
                         keep_null     = FALSE,
                         verbose       = TRUE,
                         ncores        = NULL
)
{
  # create a list containing processing parameters (used to facilitate passing
  # options to  accessory funcrtions)
  er_opts <- list(
    rast_type = rast_type,
    selbands = selbands, rastres = rastres, id_field = id_field,
    summ_data = summ_data, full_data = full_data, comp_quant = comp_quant,
    comp_freq = comp_freq,
    FUN = FUN,  small = small, na.rm = na.rm, maxchunk = maxchunk,
    join_feat_tbl = join_feat_tbl, join_geom = join_geom, keep_null = keep_null,
    verbose   = verbose, ncores = ncores, long_format = long_format
  )

  #   __________________________________________________________________________
  #   Check input types - send errors/warnings if not compliant + open the  ####
  #   in_vect if or raster file if filenames were passed instead than a *sp/*sf
  #   object or *raster object  ####

  call <- as.list(match.call())
  if (verbose) message("extract_rast --> Extracting: ", deparse(substitute(call)$in_rast),
          " data on zones of : ", deparse(substitute(call)$in_vect))

  #   __________________________________________________________________________
  #   Cast the inputs to "correct" types and do some reshuffling            ####

  in_rast   <- cast_rast(in_rast, "rastobject")
  rast_proj <- get_proj4string(in_rast)

  #   __________________________________________________________________________
  #   Identify the bands/dates to be processed                              ####

  selbands    <- er_getbands(in_rast, selbands, er_opts$verbose)
  date_check  <- ifelse(attributes(selbands)$date_check, TRUE, FALSE )
  n_selbands  <- length(selbands)
  if (date_check) {
    dates <- raster::getZ(in_rast)
  } else {
    dates <- names(in_rast)
  }
  seldates <- dates[selbands]

  # Extract bands to be processed from the original raster

  in_rast        <- in_rast[[selbands]]
  in_vect$mdxtnq <- seq(1:dim(in_vect)[1])

  #   __________________________________________________________________________
  #   Start cycling on dates/bands                                          ####

  if (n_selbands > 0) {

    # check if the id_field was passed and is correct. If not passed or not
    # correct, the record number is used as identifier in the output.

    #TODO: allow extracting also on "not univoc" features, by applying
    #dissolve_shape on "in_vect" if "id_field" is passed

    if (!is.null(id_field)) {
      if (!id_field %in% names(in_vect)) {
        warning("Invalid 'id_field' value.\nValues of the `id_feat` ",
                "column will be set to the record number of the ",
                "shapefile feature")
        id_field <- NULL
        er_opts$id_field <- NULL
      } else {
        # if (length(unique(as.data.frame(in_vect[,eval(id_field)])[,1])) != dim(in_vect)[1]) { #nolint
        #   # warning("selected ID field is not univoc ! Names of output columns",
        #   # (or values of 'feature' field if `long` = TRUE) will be set to the",
        #   # record number of the shapefile feature").
        #   # id_field <- NULL
        # }
      }
    }

    # Get the vector input and do some preprocessing. Reproject and crop
    # if necessary

    in_vect_crop  <- cast_vect(in_vect, "sfobject") %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      tibble::as_tibble() %>%
      sf::st_as_sf() %>%
      sf::st_transform(rast_proj) %>%
      crop_vect(in_rast, verbose = verbose)

    if (dim(in_vect_crop)[1] == 0) {
      stop("extract_rast --> `in_vect` does not intersect `in_rast`. Aborting!")
    }

    # Identify features cropped in the intersection
    if (dim(in_vect_crop)[1] != dim(in_vect)[1]) {
      if (verbose) {
        message(glue::glue(
          "Some features of the spatial object are outside or partially outside ",
          "the extent of the input RasterStack ! Outputs for features only ",
          "partially inside will be retrieved using only the available pixels !",
          "Outputs for features outside rasterstack extent will be set to NA.")
        )
      }

      if (!setequal(in_vect$mdxtnq, in_vect_crop$mdxtnq)) {

        outside_ids   <- setdiff(in_vect$mdxtnq, in_vect_crop$mdxtnq)
        outside_names <- ifelse(
          !is.null(id_field),
          as.character(in_vect[[eval(id_field)]][outside_ids]),
          as.character(in_vect[["mdxtnq"]][outside_ids]))
        outside_feat  <- data.frame(outside_ids   = in_vect$mdxtnq[outside_ids],
                                    outside_names = outside_names)

      } else {
        outside_feat  <- NULL
      }
    } else {
      outside_feat  <- NULL
    }

    # Now crop the raster on the vector extent. This speeds-up consistently the
    # processing in case the vectors cover only a part of the raster

    in_rast_crop <- crop_rast(in_rast, in_vect_crop, out_type = "rastobject",
                              verbose = verbose)

    # reset some useful info as that of the original file
    # TODO change "crop_rast" behaviour to keep this info from the original !
    # names(in_rast_crop) <- names(in_rast)
    # if (date_check) in_rast_crop <- raster::setZ(in_rast_crop,
    #                                              raster::getZ(in_rast))

    #   ________________________________________________________________________
    #   Extract values if the zone pbject is a point shapefile              ####
    #   TODO: extraction on LINES ! )

    if (inherits(sf::st_geometry(in_vect), "sfc_POINT")) {
      # Convert the zone object to *Spatial to allow use of "raster::extract"
      out_list <- er_points(in_vect_crop,
                            in_vect,
                            in_rast_crop,
                            seldates,
                            selbands,
                            n_selbands,
                            date_check,
                            er_opts,
                            outside_feat)

      # end processing on points

    } else {

      #   ______________________________________________________________________
      #   Extract values if the zone object is a polygon                    ####
      out_list <- er_polygons(in_vect_crop,
                              in_vect,
                              in_rast_crop,
                              seldates,
                              selbands,
                              n_selbands,
                              date_check,
                              er_opts,
                              outside_feat)

    }

    return(out_list)
  } else {
    stop("Selected time range/bands do not overlap with the ",
         "rasterStack input dataset!\n Doing Nothing !")
  }
}



