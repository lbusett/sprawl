
#' lb_writeenvits
#' @description Function to save an "R" multitemporal raster object (e.g.,
#'   rasterStack) to an ENVI multiband file (BSQ). Bandnames and wavelengths of
#'   the output ENVI file are set on the basis of the acquisition dates.
#'   In paricular, wl takes the value of the acquisition doy, computed starting
#'   from 1st of January of the minimum year in the time serties
#'
#' @param in_raster Input "R" raster object
#' @param in_dates Dates corresponding to the different dates of acquisition
#'   (as "Dates" array or numeric array of doys)
#' @param out_file Output file name
#' @param dtype string data type of output (e.g., 'INT2s','FLT4S', etc. - see
#'   help of "writeRaster for a list). Defaults to floating.
#' @return NULL
#' @export
#'
#' @import raster tools
#'
#' @examples
#' \dontrun{
#'  # Create a raster stack from a list of single date raster
#'  #List of files
#'  in_files = file_path_sans_ext(list.files(in_folder, pattern = '*.tif$',
#'    full.names = T))
#'  # List of acquisition dates - MUST be same order as files
#'  in_dates = as.Date(c('2000-01-01','2000-01-31',..... ))
#'  out_raster = 'd:/temp/out_multitemp'
#'  # Create the ENVI raster file
#'  lb_writeenvits (in_raster = raster_in, in_dates = acq_dates,
#'    out_file = out_raster)
#'}

lb_writeenvits = function(in_raster,
                          in_dates,
                          out_file,
                          dtype = 'FLT4S') {

  out_raster = writeRaster(in_raster, filename = out_file,
                           overwrite = T, format = 'ENVI',datatype = dtype)
  bandnames = paste(basename(file_path_sans_ext(out_file)),
                    in_dates, sep = '_')
  wl = as.numeric(lb_datetodoy(in_dates) +
                    365 * (year(in_dates)-min(year(in_dates))))
  hdrfile = paste0(file_path_sans_ext(out_file),'.hdr')
  write(paste("Band Names = {", paste(bandnames, collapse = ', '), "}",
              sep=""), file = hdrfile, append = TRUE)
  write(paste("wavelength = {", paste(wl, collapse = ', '), "}", sep = ""),
        file=hdrfile,append = TRUE)
  gc()
}
