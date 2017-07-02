#' convert_BS_timeseries
#'
#' @param in_files
#' @param in_dates
#' @param out_rast
#' @param shp_in
#'
#' @return
#' @export
#'

convert_BS_timeseries = function(in_files,in_dates, out_rast = out_rast, shp_in){
  out_rast_temp = paste0(out_rast,'.vrt')
  gdalbuildvrt(in_files, out_rast_temp, separate = T, overwrite = T, allow_projection_difference = T)
  in_rast = stack(out_rast_temp)     # create rasterstack timeseries object
  r = calc(in_rast, fun = function(x){as.integer(100*log10(x))})
  lb_writeenvits(in_raster = r, in_dates = in_dates, out_file = out_rast, dtype = 'INT2S')
  gc()
}
