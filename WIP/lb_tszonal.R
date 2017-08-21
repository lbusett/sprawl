#' lb_tszonal
#'
#' @description Function for extracting zonal statistics from a time series of raster data (avg, stdev, variation coefficient, min and max) over the spatial feeatures (points, polygons) of a shapefile or "R" *Spatial
#' object. Output is a list of data frames containing a date column, a doy coulumn, and then a column for each spatial feature (e.g., polygon) from
#' which statistics where extracted. Input can be a list of coregistered single date files, or a multitemporal raster. User can select to save a
#' multitemporal raster in output, cropped on the extent of the input spatial object. See "details" for other info.
#'
#' @details This function can be used to extract statistic information (mean, sd, etc) for the different
#' polygons/points of a shapefile/spatial object from a list of raster images acquired in different dates, or
#' from a multiband raster file (In both cases, acquisition dates must be provided).
#' The output is a list of data frames. Number of elements in the list is equal to the number of statistics computed (e.g., if only avg
#' is selected, then the list has one entry. If avg, sd and cvar are selected, then the list has three elements).
#' Each statistics data frame contains a date column, a doy coulumn, and then a column for each spatial feature (e.g., polygon) from
#' which statistics where extracted. See the examples to see how to access simply to the results.
#'
#' NOTE: If out_rast is specified, a new multiband raster file containing the raster time series cropped on the extent of the
#' input spatial object is created
#'
#' NOTE 2: Behaviour differs according to the type of vector file used. For polygons, values are computed considering pixels which centroid is
#' included in the polygon. For lines, all pixels  that are touched by the line are considered.
#' If shp represents points, only the cells on which the point falls are used.
#' User can specify if applying a buffer on the shapefile before extracting the data
#'
#' NOTE 3: If shpaefile and raster files have different projections, shapefile is automatically reprojected
#'
#' NOTE 4: ALL input raster files must have the same dimensions !!!!!
#'
#' @param in_files list of raster images from which statistics has to be computed OR single multiband raster file
#' @param in_dates acquisition dates of the raster files (same order of the raster files). If input is a multiband, the number of
#' elements of in_dates must be equal to the number of bands
#' @param shp "R" spatial object OR filename of ESRI shapefile  containing polygons on which statistics must be extracted
#' @param id_field field of the shapefile/spatial object containing unique identifiers (names, or ids) of the spatial elements for which
#' statistics has to be extracted
#' @param BS logical if TRUE, then the time serie is a BackScatter SAR time serie. In this case, values are converted to 10*log10
#' @param format format of output imagery. default = ENVI. All gdal formats possible (e.g., "Gtiff)
#' @param avg logical if T, compute average. Default = T
#' @param sd logical if T, compute standard deviation. Default = F
#' @param minmax logical if T, compute minimum and maximum.Default = F
#' @param cvar logical if T, compute variation coefficient. Default = F
#' @param out_rast character (Optional). If specified, a new multiband raster file containing the time series cropped on extent of the
#' input spatial object is created
#' @param start_date Date, posIXct, posIXlt Starting date for extraction. Default = NULL (Start from first available date)
#' @param end_date Date, posIXct, posIXlt Ending date for extraction. Default = NULL (End on Last available date)
#' @param verbose logical If T, send out progress messages (default = T)
#' @param na.rm logical If T, remove nodata while computing statistics (default = T)
#' @param in_nodata numeric value of NODATA in input raster files
#' @param buffer numeric if specified, a buffer of specified width is applied to the spatial object before computing
#' statistics. Negative values allow to "shrink" the polygons.
#'
#' @return list of data frames. Number of elements in the list is equal to the number of statistics computed (e.g., if only avg
#' is selected, then the list has one entry. If avg, sd and cvar are selected, then the list has three elements).
#' Each statistics data frame contains a date column, a doy coulumn, and then a column for each spatial feature (e.g., polygon) from
#' which statistics where extracted.
#' @export
#'
#' @importFrom data.table raster rgdal rgeos gdalUtils rts
#'
#' @examples
#' \dontrun{# Extract average and standard deviation values from a list of raster datasets
#' # for each polygon of a shapefile, after applying a 30m buffer, for each date available.
#' the id field is used as identifier of polygons
#' #Set the inputs
#' in_files = file_path_sans_ext(list.files(in_folder, pattern = '*.hdr$', full.names = T))   #List of files
#' in_dates = as.Date(c('2000-01-01','2000-01-31',..... ))    # List of acquisition dates (MUST be same order as files)
#' shpname = "D:/polygons.shp"  # Path to Polygon Shapefile
#' out_rast = "D:/out_rast_multiband"  # Name of output multitemporal file (optional)
#'
#' #Compute statistics
#' stats = lb_tszonal(in_files= in_files,in_dates = in_dates, id_field = 'id', shp = shpname,
#'                    buffer = -30, out_rast = out_rast, sd = T, cvar = T)
#'
#' #Access results
#' avg_data = stats$avg   # Get all avg data (data frame)
#' sd_data = stats$sd  # Get all sd data (data frame)
#'
#' avg_data_pol = avg_data$'id_of_polygon'   # extract data for just one polygon using its id
#' sd_data_pol = sd_data$'id_of_polygon'
#'
#' plot(avg_data_pol$avg, avg_data_pol$doy)  # plot average vs date for selected polygon
#' }

lb_tszonal = function(in_files, in_dates, shp,id_field, buffer = NULL, BS = F, format = 'ENVI',
                        avg = T, sd = F, minmax = F, cvar = F ,out_rast = NULL, start_date = NULL,
                        end_date = NULL,verbose = T, na.rm = T, in_nodata = NULL){
  # beginCluster()

  if(class(shp) == 'character'){shp = readshape(shp)}

  shp_ok = spTransform(shp, proj4string(raster(in_files[1])))  # Transform shapefile in proj of raster

  if (length(buffer == 1)) { shp_ok = gBuffer(shp_ok, byid=T, id=shp_ok@data[,eval(id_field)],width=buffer)}

  # if (length(out_rast) == 0) {out_rast = paste0(dirname(in_files[1]), 'temp_vrt.vrt')}   # If no raster ts output, create a temporary vrt filename to be then removed

  # gdalbuildvrt(in_files, out_rast, separate = T,       # create temporary cropped ts file (vrt)
  #              te = c(extent(shp_ok)@xmin,extent(shp_ok)@ymin,extent(shp_ok)@xmax,extent(shp_ok)@ymax), overwrite = T, allow_projection_difference = T)
  in_rast = rts(crop(stack(in_files), extent(shp_ok)), time = in_dates)     # create rasterstack timeseries object
  if(length(in_nodata) == 1) {NAvalue(in_rast) = in_nodata}
  if (BS == T) {        # If backscatter series, compute logarithm of values
    values(in_rast@raster) = 10*log10( values(in_rast@raster))
    if (length(out_rast) ==1) {
      if (format == 'ENVI') {    # if output format envi, update the header with band names and wavelengths (doys)
        lb_write_envits(in_raster = in_rast@raster, in_dates = in_dates, out_file = out_rast)
      } else {writeRaster(in_rast@raster, filename = out_rast,overwrite = T, format = format)}
    }
  } else {  # If not backscatter series, keep the original values of the time series

    if (length(out_rast) ==1) {
      if (format == 'ENVI') {    # if output format envi, update the header with band names and wavelengths (doys)
        lb_write_envits(in_raster = in_rast@raster, in_dates = in_dates, out_file = out_rast)
      } else {writeRaster(in_rast@raster, filename = out_rast,overwrite = T, format = format)}
    }
  }

  # From here onwards, take the raster time series and compute the zonal statistics
browser()
  zone_raster = rasterize(shp_ok, in_rast@raster[[1]], id_field)    # rasterize the shapefile
  zones = getValues(zone_raster)   # get zones values
  ok_zones = which(is.finite(zones))  # find good zones
  zones = zones [ok_zones]

  if (length(start_date == 1) & length(end_date) ==1) {   # if dates selected, find the bands within the selected range
    sel_indexes = which(index(in_rast) >= start_date & index(in_rast) <= end_date)
  } else {
    sel_indexes = seq(1:length(index(in_rast)))
  }
  ncols = length(unique(zones))+2
  dim_out =  matrix(nrow = length(sel_indexes), ncol = ncols)
  in_doys = lb_datetodoy(in_dates)

  # prepare matrixes to contain the results of avg and stdev
  if (avg) {ts_avg = dim_out}
  if (sd) {ts_sd = dim_out}
  if (minmax) {ts_min = dim_out ; ts_min = dim_out}
  if (cvar) {ts_cvar = dim_out}
  # browser()
  for (f in 1:length(sel_indexes)) {    # extract values for the different zones and dates
    if (verbose == T) {print(paste0('Extracting data from date: ', index(in_rast)[sel_indexes[f]]))}
    value = getValues(in_rast@raster[[sel_indexes[f]]]) [ok_zones]
    rDT <- data.table(value, zones)
    setkey(rDT, zones)
    if (avg == TRUE) {
      ts_avg[f,3:ncols] = rDT[, lapply(.SD,mean, na.rm = na.rm), by=zones]$value
    }
    if (sd) {ts_sd[f,3:ncols] = rDT[, lapply(.SD, sd, na.rm = na.rm), by=zones]$value}
    if (minmax) {
      ts_min[f,3:ncols] = rDT[, lapply(.SD, min, na.rm = na.rm), by=zones]$value
      ts_max[f,3:ncols] = rDT[, lapply(.SD, max, na.rm = na.rm), by=zones]$value
    }
    if (cvar) {ts_cvar[f,3:ncols] = ts_sd[f,]/ts_avg[f,]}
  }

  if (avg) {
    ts_avg = as.data.frame(ts_avg)    # convert to data frame
    names(ts_avg) = c('date','doy',unique(rDT$zones))# put the "id_field" values as names for the different columns
    ts_avg$date = in_dates
    ts_avg$doy = in_doys
  }
  if (sd) {
    ts_sd= as.data.frame(ts_sd)    # convert to data frame
    names(ts_sd) = c('date','doy',unique(rDT$zones)) # put the "id_field" values as names for the different columns
    ts_sd$date = in_dates
    ts_sd$doy = in_doys
  }

  if (minmax) {
    ts_min = as.data.frame(ts_min) ; ts_max = as.data.frame(ts_max)
    names(ts_min) = names(ts_max) = c('date','doy',unique(rDT$zones))
    ts_min$date = in_dates
    ts_max$doy = in_doys
  }
  if (cvar) {
    ts_cvar = as.data.frame(ts_cvar)
    names(ts_cvar) =  c('date','doy',unique(rDT$zones))
    ts_cvar$doy = in_doys
  }
  out_list = list()

  if(avg) {out_list[['avg']] = ts_avg}
  if(sd) {out_list[['sd']] = ts_sd}
  if(minmax) {out_list[['min']] = ts_min  ;  out_list[['max']] = ts_max}
  if(cvar) {out_list[['cvar']] = ts_cvar}

  return(out_list)  # return results
}
