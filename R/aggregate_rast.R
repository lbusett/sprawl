#' @title aggregate_rast
#' @description Function used to extract values of a single- or multi- band
#'   raster for pixels corresponding to the features of a vector (Polygon,
#'   Point or Line)
#' @param in_val_rast Object of class `Raster*` or filename corresponding to a
#'   valid raster file
#' @param in_zones_rast PARAM_DESCRIPTION
#' @param FUN PARAM_DESCRIPTION, Default: mean
#' @param maxchunk PARAM_DESCRIPTION, Default: 5e+07
#' @param method PARAM_DESCRIPTION, Default: 'fastdisk'
#' @param out_file PARAM_DESCRIPTION, Default: NULL
#' @param overwrite `logical` If TRUE, existing files are ovewritten,
#'   Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  in_file <- system.file("extdata/MODIS_test", "EVIts_test.tif",
#'                                          package = "sprawl.data")
#'  in_rast      <- read_rast(in_file, bands = 20)
#'  tempraster   <- tempfile(fileext = ".tif")
#'  in_obj_zones <- raster::aggregate(in_rast,
#'                                      fact      = 4,
#'                                      filename  = tempraster,
#'                                      overwrite = T)
#'  test <- aggregate_rast(in_rast,
#'                         in_obj_zones,
#'                         FUN     = mean,
#'                         method  = "fastdisk",
#'                         to_file = FALSE,
#'                         verbose = FALSE)
#'  }
#' @rdname aggregate_rast
#' @export
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#' @importFrom data.table data.table
#' @importFrom raster raster writeRaster rasterize res
#' @importFrom sf st_crs st_transform st_bbox st_coordinates st_centroid st_as_sf
#'  st_set_crs
#' @importFrom sp proj4string

aggregate_rast <- function(in_val_rast,
                           in_zones_rast,
                           FUN       = mean,
                           maxchunk  = 50E6,
                           method    = "disk",
                           out_type  = "rastobject",
                           out_file  = NULL,
                           nodata_in = NULL,
                           verbose   = TRUE,
                           overwrite = FALSE) {

  call <- match.call()

  geometry <- myfun <- NULL

  assertthat::assert_that(
    method %in% c("disk", "memory"),
    msg = "aggregate_rast --> `method` must be either \"disk\" or \"memory\". Aborting!" #nolint
  )

  assertthat::assert_that(
    out_type %in% c("rastobject", "rastfile"),
    msg = "aggregate_rast --> `out_type` must be either \"rastobject\" or \"rastfile\". Aborting!" #nolint
  )

  assertthat::assert_that(
    is.numeric(maxchunk),
    msg = "aggregate_rast --> `max_chunk` must be numeric. Aborting!" #nolint
  )

  # Check if the arguments are `Raster*` objects or files. Abort on failure ####
  in_type       <- get_rastype(in_val_rast)
  in_zones_type <- get_rastype(in_zones_rast)

  if (verbose) message("aggregate_raster --> Aggregating values of `",
                       call[[2]], "` on cells of `", call[[3]], "`")

  #   __________________________________________________________________________
  #   Compute aggregated values using `sprawl::extract_rast`                ####
  if (verbose) message("  --> Creating Fishnet on zones_rast")

  in_fish <- create_fishnet(in_zones_rast,
                            cellsize = raster::res(in_zones_rast),
                            exact_csize = TRUE,
                            verbose = FALSE)

  if (verbose) message("  --> Extracting values of in_val_rast", #nolint
                       " on cells of in_zones_rast")

  agg_values   <- extract_rast(in_val_rast,
                               in_fish,
                               full_data = FALSE,
                               maxchunk  = maxchunk,
                               FUN       = FUN,
                               id_field  = "cell_id",
                               join_feat_tbl = FALSE,
                               join_geom = TRUE,
                               verbose   = FALSE)$stats

  #   ____________________________________________________________________________
  #   reproject to crs of zones_rast if needed                                ####

  if (sf::st_crs(agg_values) != sf::st_crs(in_fish)) {
    agg_values <- sf::st_transform(agg_values, sf::st_crs(in_fish))
  }

  #   __________________________________________________________________________
  #   Build a new raster using the aggregated values                        ####
  #
  if (verbose) message("aggregate_raster --> Assigning extracted values to a ",
                       "new raster")

  if (is.null(out_file)) {
    out_file <- tempfile(fileext = ".tif")
  }

  #   __________________________________________________________________________
  #   method "disk": create a temporary shapefile and use gdal_rasterize    ####
  #   to rasterize it

  if (method == "disk") {
    # create a temporary shapefile
    tempvecfile      <- tempfile(fileext = ".shp")
    write_shape(agg_values["myfun"], tempvecfile, overwrite = TRUE)
    rasterize_string <- paste("-a myfun",
                              "-tr ", paste(raster::res(in_zones_rast),
                                            collapse = " "),
                              "-te ", paste(sf::st_bbox(in_fish),
                                            collapse = " "),
                              "-co COMPRESS=DEFLATE",
                              tempvecfile,
                              out_file)
    rastout <- suppressWarnings(try(system2(file.path(find_gdal(),"gdal_rasterize"),
                                            args = rasterize_string,
                                            stdout = NULL, stderr = TRUE),
                                    silent = T))

    if (!is.null(attr(rastout, "status"))) {

      stop("aggregate_rast --> An error occurred while rasterizing results.",
           " The call to gdal_rasterize was: gdal_rasterize ",
           substitute(rasterize_string))

    }
  }

  if (method == "memory") {
    #   __________________________________________________________________________
    #   method "memory": compute centroids of the cells, and use              ####
    #   raster::rasterize to write it

    agg_values <- data.table::data.table(agg_values)
    out  <- agg_values[,{
      est <- sf::st_coordinates(sf::st_centroid(geometry))
      list(X = est[,1], Y = est[,2], Z = myfun)}]   %>%
      sf::st_as_sf(coords = c("X","Y")) %>%
      sf::st_set_crs(sp::proj4string(in_zones_rast)) %>%
      as("Spatial")

    raster::rasterize(out,
                      in_zones_rast,
                      field     = "Z",
                      filename  = out_file,
                      options   = "COMPRESS=DEFLATE",
                      overwrite = overwrite)
  }

  if (out_file == "rastfile") {
    return(out_rast)
  } else {
    return(read_rast(out_file))
  }

}
