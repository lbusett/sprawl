#' @title aggregate_rast
#' @description Function used to extract values of a single- or multi- band raster for pixels
#' corresponding to the features of a vector (Polygon, Point or Line)
#' @param in_rast_values PARAM_DESCRIPTION
#' @param zones_rast PARAM_DESCRIPTION
#' @param FUN PARAM_DESCRIPTION, Default: mean
#' @param maxchunk PARAM_DESCRIPTION, Default: 5e+07
#' @param method PARAM_DESCRIPTION, Default: 'fastdisk'
#' @param to_file PARAM_DESCRIPTION, Default: FALSE
#' @param out_file PARAM_DESCRIPTION, Default: NULL
#' @param nodata_out PARAM_DESCRIPTION, Default: NA
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{data.table}}

#'  \code{\link[raster]{raster}},\code{\link[raster]{writeRaster}},\code{\link[raster]{rasterize}}

#'  \code{\link[sf]{st_crs}},\code{\link[sf]{st_transform}},\code{\link[sf]{st_bbox}},\code{\link[sf]{st_coordinates}},\code{\link[sf]{st_centroid}},\code{\link[sf]{st_as_sf}},\code{\link[sf]{st_set_crs}}

#'  \code{\link[sp]{proj4string}}
#' @rdname aggregate_rast
#' @export
#' @author Lorenzo Busetto, PhD (2017) email: <lbusett@gmail.com>
#' @importFrom data.table data.table
#' @importFrom raster raster writeRaster rasterize
#' @importFrom sf st_crs st_transform st_bbox st_coordinates st_centroid st_as_sf st_set_crs
#' @importFrom sp proj4string

aggregate_rast <- function(in_rast_values,
                           zones_rast,
                           FUN      = mean,
                           maxchunk = 50E6,
                           method   = "fastdisk",
                           to_file  = FALSE,
                           out_file = NULL,
                           nodata_out = NA,
                           verbose  = TRUE ){

  #   ____________________________________________________________________________
  #   Check arguments and recast if needed                                    ####

  if (verbose) message("aggregate_raster --> Checking Arguments")

  #   ____________________________________________________________________________
  #   Compute aggregated values using `sprawl::extract_rast` or               ####
  #   `sprawl::comp_zonestats`
  if (verbose) message("aggregate_raster --> Creating Fishnet on zones_rast")

  in_fish <- create_fishnet(zones_rast,
                                 cellsize = res(zones_rast)[1])
  if (verbose) message("aggregate_raster --> Aggregating values of in_rast_values on
                      cells of zones_rast")

  agg_values   <- extract_rast(in_rast_values,
                               in_fish,
                               full_data = FALSE,
                               verbose   = verbose,
                               mode      = "std",
                               maxchunk  = maxchunk,
                               FUN       = FUN,
                               id_field  = "cell_id")$stats
  # if (!is.na(nodata_out)) {
    where_na <- which(is.na(agg_values$myfun))
    agg_values$myfun[where_na] <- nodata_out
  # } else {
    # agg_values$myfun[where_na] <- NA
  # }

  if (sf::st_crs(agg_values) != sf::st_crs(in_fish)) {
    agg_values <- sf::st_transform(agg_values, sf::st_crs(in_fish))
  }

  #   ____________________________________________________________________________
  #   Build a new raster using the aggregated values                          ####
  #
  if (verbose) message("aggregate_raster --> Assigning extracted values to a new raster")

  if (to_file & !is.null(out_file)) {
    teamprastfile <- out_file
  } else {
    teamprastfile <- tempfile(fileext = ".tif")
  }

  if (method == "fastdisk") {
    tempvecfile  <- tempfile(fileext = ".shp")

    if (is.na(nodata_out)) {
      raster::raster(zones_rast) %>%
        raster::writeRaster(filename = teamprastfile,
                            overwrite = TRUE)
    } else {
      raster::raster(zones_rast) %>%
        raster::writeRaster(filename = teamprastfile,
                            overwrite = TRUE, NAflag = nodata_out)
    }

    write_shape(agg_values,tempvecfile, overwrite = T)
    rasterize_string <- paste("-a myfun",
                              "-tr ", paste(res(zones_rast), collapse = " "),
                              "-te ", paste(sf::st_bbox(in_fish), collapse = " "),
                              "-co COMPRESS=DEFLATE",
                              ifelse(!is.na(nodata_out), paste0("-a_nodata ", nodata_out),
                                     paste0("-a_nodata NoData")),
                              tempvecfile,
                              teamprastfile)
    system2(file.path(find_gdal(),"gdal_rasterize"), args = rasterize_string, stdout = NULL)

    if (!to_file) {
      out_rast <- raster(teamprastfile)
    }
  } else {
    agg_values <- data.table::data.table(agg_values)
    out        <- agg_values[,
                             {est <- sf::st_coordinates(sf::st_centroid(geometry))
                             list(X = est[,1], Y = est[,2], Z = myfun)}
                             ]   %>%
      sf::st_as_sf(coords = c("X","Y")) %>%
      sf::st_set_crs(sp::proj4string(zones_rast)) %>%
      as("Spatial")
    # browser()
    out_rast <- raster::rasterize(out, zones_rast, field = "Z")
    if (to_file) {

      writeRaster(out_rast,
                  teamprastfile,
                  options = c("COMPRESS=DEFLATE"),
                  overwrite = TRUE,
                  NAflag = nodata_out)
    }
  }
  if (to_file) {
    if (method == "fastdisk") file.remove(tempvecfile)
    out_rast <- teamprastfile
  } else {
    # out_rast <- raster::raster(teamprastfile)
    if (method == "fastdisk") file.remove(tempvecfile)
    # file.remove(teamprastfile)
  }
  return(out_rast)
}
