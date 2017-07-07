# LB_Aggregate_to_ERMES_GRID_batch
# ;:Description -- batch version - to be called from IDL !
# ;
# ; This function, given in input a single band raster file name (ENVI or GTiff format), an indication of its projection and the file name corresponding to a polygon shapefile representing the boundaries
# ; of the ERMES grid cells (in LAEA projection) does the following:
#   ;
# ;   1) Reproject the image into the ETR89/LAEA projection and saves the output in the out_folder_raster folder
# ;   2) Aggregates the values of the input file on the ERMES 2x2 Km grid (computes average of "legal" values of the input raster within each cell and their standard deviation )
# ;   3) Writes the average and standard deviation values retrieved as a "new" envi or GTiff file with 2x2 km resolution, correspondent to the ERMES grid
# ;   4) Writes the ouputs also as an RData spatialpointsdataframe file (useful for later elaborations)
# ;
# ;:RETURNS:
#   ;
# ;   NONE - new raster and RData files are written in specific subfolders of out_folder
# ;
# ;:AUTHOR: Lorenzo Busetto, phD (2014)
# ;  #' email: busetto.l@@irea.cnr.it
# ;
# ;License GPL(>2)

# args <- commandArgs(TRUE)
# t1 = Sys.time()
# args
# # load required libraries (install if missing)
# pkg_list = c('tools','raster','sp', 'gdalUtils','rgdal','data.table','plyr', 'utils')
# pkg_test <- function(x) {
#   if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)
#     require(x,character.only=TRUE)}
# }
# for (pkg in pkg_list) {pkg_test(pkg)}
#
# memory.limit(8000)
# #   rasterOptions (setfileext = F)
#
# # Retrieve parameters from the caller
# in_raster_file = args[1]
# in_nodata = args[2]
# in_raster_proj = args[3]
# in_ermes_grid_laea  = args[4]
# in_lc_file  = args[5]
# out_folder = args[6]
# out_filename = args[7]
# out_format = args[8]
#
# fc_threshold = as.numeric(args[9]).
#
#
library(raster)
library(sf)
library(gdalUtils)
library(data.table)

in_raster_file     <- ("/home/lb/Temp/buttami/MOD15/LAI_8Days_500m_v6/Lai/MCD15A2H_Lai_2017_137.dat")
QA_file            <- raster("/home/lb/Temp/buttami/MOD15/LAI_8Days_500m_v6/QC_SCF/MCD15A2H_QC_SCF_2017_137.dat")
QC_bits            <- raster("/home/lb/Temp/buttami/MOD15/LAI_8Days_500m_v6/QC_bits/MCD15A2H_QC_bits_2017_137.dat")
in_ermes_grid_laea <- "/home/lb/Temp/buttami/MOD15/old_code/IT_ERMES_Regional_Grid.shp"
in_lc_file         <- "/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif"


   file_in <- in_raster_file
   inrast  <- raster(in_lc_file)

   ext_rast <- extent(inrast)
   csize    <- res(inrast)[1]

  out_grid_500file <- "/home/lb/Temp/buttami/MOD15/grid_500.shp"
  out =  create_fishnet(inrast, cellsize = csize, out_shape = out_grid_500file,
          overwrite = TRUE)

in_grid <- readshape(in_ermes_grid_laea)
in_grid2 = in_grid[5500:6500,] %>%
  as("Spatial")
tempgrid <- writeshape(in_grid2, "/home/lb/Temp/buttami/MOD15/smallgrid_250.shp")
aa = readshape("/home/lb/Temp/buttami/MOD15/smallgrid_250.shp")
mapview(in_grid2)

out = comp_zonal(raster(in_lc_file),"/home/lb/Temp/buttami/MOD15/grid_500.shp",full_data = FALSE,verbose = TRUE,ncores = 4, mode = "std", maxchunk = 506)





in_lc_file = "/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE.tif"
aggregate_in_lc_file = "/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE_aggregated.tif"
in_nodata = 255
in_raster_proj = 'MODIS_SIN'
in_lc_raster_proj = CRS("+init=epsg:32632")
laea_crs = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
in_ermes_grid_laea  = "/home/lb/Temp/buttami/MOD15/old_code/IT_ERMES_Regional_Grid.shp"
out_ermes_grid_reproj  <-'/home/lb/Temp/buttami/MOD15/old_code/IT_ERMES_Regional_Grid_reproj.shp'
grid_rasterized_laea = "/home/lb/Temp/buttami/MOD15/old_code/IT_ERMES_Regional_Gridraster.tif"
# Define Proj4 strings for warping
in_raster_file_proj4 = CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')
aggregate_lc_RData_file = "/home/lb/Temp/buttami/MOD15/Italy_mask_ARABLE_aggregated.RData"

masked_in_raster_file <- raster(in_raster_file)
masked_in_raster_file[masked_in_raster_file > 249] = NA
masked_in_raster_file[QA_file != 0] = NA
masked_in_raster_file[QC_bits > 1] = NA
masked_in_raster_file <- masked_in_raster_file * 0.1


#out_folder = 'D:/Documents/ERMES/Documents/Data_Processing/EO_Data_Aggregation/new_tests'
#out_filename = 'D:/Documents/ERMES/Documents/Data_Processing/EO_Data_Aggregation/new_tests'
out_format<- 'GTiff'

temp_res <- c(46.3312,46.3316)
ERMES_cells_poly <- readshape(in_ermes_grid_laea)

if(file.exists(out_ermes_grid_reproj) == FALSE) {

  ERMES_cells_poly_reproj = st_transform(ERMES_cells_poly, proj4string(in_raster_file)) %>%
    as("Spatial")
  writeshape(ERMES_cells_poly_reproj, out_ermes_grid_reproj, overwrite = TRUE) #save the shapefile

} else {
  ERMES_cells_poly_reproj = readshape(out_ermes_grid_reproj, as_sp = T)
}

ERMES_cells_poly   <- as(ERMES_cells_poly, "Spatial")
extent_poly_laea   <- extent(ERMES_cells_poly)   # Retrieve extnt of ERMES grid in original LAEA proj
extent_poly_reproj <- extent(ERMES_cells_poly_reproj)  # Retrieve extnt of ERMES grid in the projection of input raster

# Reproject the original raster to LAEA projection , clip it on ERMES Grid extent
out_file_raster <- tempfile(fileext = ".tif")
# rast_reproj = gdalwarp(in_raster_file,out_file_raster, s_srs = in_raster_file_proj4, t_srs = as.character(laea_crs),
#          r = "near", te = c(extent_poly_reproj@xmin, extent_poly_reproj@ymin,extent_poly_reproj@xmax, extent_poly_reproj@ymax),
#          of = out_format, overwrite = T, output_Raster = T)
tempmaskraster <- tempfile(fileext = ".tif")
  writeRaster(masked_in_raster_file, tempmaskraster)
temp_raster_lai = gdalwarp(tempmaskraster,out_file_raster, s_srs = in_raster_file_proj4, t_srs = as.character(laea_crs),
                       r = "near", of = out_format, overwrite = T, output_Raster = T)


# NEW ------------------
# Create a polygon reference Grid for the lc file, to be used to compute the arable_fc in each cell of the input raster
# of each cell of the original input LAI image. Then "convert" it to a raster with values equal to the "id" of the grid

if (file.exists(aggregate_lc_RData_file) ==F ){

  rtemp = raster(out_file_raster)		# get input resized on study area
  grid = getGridTopology(as(rtemp, "SpatialGrid"))		# create a grid
  pix_grid = SpatialGrid(grid,laea_crs)
  pix_grid_cell = SpatialGridDataFrame(pix_grid, data = data.frame(id = seq(1:(dim(rtemp)[1]*dim(rtemp)[2]))),laea_crs)
  sp_polygons_lai = as(pix_grid_cell, "SpatialPolygonsDataFrame")	#create the shape - convert grid to polygons
  # write to shapefile
  out_file_lc_shape <- tempfile(fileext = ".shp")
  writeshape(sp_polygons_lai, out_file_lc_shape,  overwrite = T) #save the shapefile
  extent_poly_lai = extent(sp_polygons_lai)
  out_file_lc_grid <- tempfile(fileext = ".tif")
  temp_raster_cells_lc <- gdal_rasterize(out_file_lc_shape,out_file_lc_grid,a = "id", output_Raster = T,
                 te = c(extent_poly_lai@xmin, extent_poly_lai@ymin,extent_poly_lai@xmax, extent_poly_lai@ymax),
                 tr = temp_res, tap = F, output_Raster = T)
  data_raster_cells_lc = getValues(temp_raster_cells_lc)

  # NEW --------------
  # Compute the arable_fc for each pixel of the raster, and save the fc to a RData file

  # Get the values of the "dummy" raster of cells codes
  in_lc_rast = raster(in_lc_file)
  in_lc_raster_proj = CRS(proj4string(in_lc_rast))
  tempfile_raster_lc <- tempfile(fileext = ".tif")
  # Create a temporary raster file with higher spatial resolution than the original, resized on the extent of the ERMES reference grid
  temp_raster_lc <- gdalwarp(in_lc_file,tempfile_raster_lc, s_srs = in_lc_raster_proj, t_srs = laea_crs,
           tr = temp_res, r = "near", te = c(extent_poly_lai@xmin, extent_poly_lai@ymin,extent_poly_lai@xmax, extent_poly_lai@ymax),
           dstnodata =  255,  overwrite = T, tap = F, output_Raster = T)

  # temp_raster_lc <- raster(file.path(temp_folder, 'temporary_raster_lc.tif'))
  in_nodata = 255
  # temp_raster_lc <- raster(file.path(temp_folder, 'temporary_raster_lc.tif'))
  data_raster_lc = getValues(temp_raster_lc)
  nodata_pixs = which(data_raster_lc == in_nodata)		# set NODATA to NA
  data_raster_lc [nodata_pixs] = NA

  # Create a Data.table with two columns. First column taken from the cell_codes raster - tells to which cell each pixel correspond
  # Second column taken from the increased resolution input lc zraster - tells which lc values correspond to each cell

  cells_dt_lc = data.table(int_id = data_raster_cells_lc, value = data_raster_lc)
  names(cells_dt_lc)[2] = "value"
  setkey(cells_dt_lc, int_id)  # Set a indexing key to increase speed of zonal statistics computation
  aggregate_values_lc = cells_dt_lc[, list(average = as.numeric(mean(value, na.rm = T)),
                                           n_cells = as.numeric(length(value))
  ), by = key(cells_dt_lc)]
  cell_codes = unique(cells_dt_lc$int_id)
  good_codes = seq(1:max(cell_codes))
  aggregate_values_lc = aggregate_values_lc[.(good_codes)] # Remove rows corresponding to areas outside ERMES grid

  save(aggregate_values_lc, file = aggregate_lc_RData_file)

}

# - ---------------------------------------------------------------
# --- NEW --- set to NA the cells of the original raster with fc_arable values lower than the fc threshold
# - ---------------------------------------------------------------
aggregate_values_lc = get(load(aggregate_lc_RData_file))  # load the arable_fc data from the RData file
masked_pixs = which(aggregate_values_lc$average < fc_threshold | is.na(aggregate_values_lc$average) == T)		# set NODATA to NA
temp_raster_lai[masked_pixs] = NA
out_temp_raster_masked = tempfile("temp_masked_LAI_file", fileext = ".tif")
writeRaster(temp_raster_lai , out_temp_raster_masked, NAflag = as.numeric(in_nodata), overwrite = T)

# REmove NODATA from input and apply scale factor
# temp_raster_masked <- raster(out_temp_raster_masked)



# Specify resolution for temporary raster files --> If input proj is geographic, set it to 0.0008928571 (about 50 meters)
# if input proj is metric of whatever kind, set it to 46.3312 meter (it is a submultiple of orignal MODIS resolutions)

# if (in_raster_file_proj4 == '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0,0,0,0') {
#   temp_res = c(0.0004464286, 0.0004464286)} else {temp_res = c(46.3312,46.3312)}

# Set paths

# out_folder_RData = file.path(out_folder,'ERMES_Grid/RData')
# out_folder_raster = file.path(out_folder,'Reprojected_Native_Res')
# out_folder_raster_aggregated = file.path(out_folder,'ERMES_Grid/Raster')
# #out_folder_raster_aggregated_stdev = file.path(out_folder,'ERMES_Grid/Raster/Stdev')
# temp_folder = file.path(out_folder,'Temp_Data')
# in_raster_file_masked = file.path(temp_folder,'maske_in_raster.tif')
#
# out_file_RData = file.path(out_folder_RData, paste(basename(in_raster_file),'_2x2Grid.RData', sep = ''))
# out_file_RData_lc = file.path(out_folder_RData, paste(basename(in_raster_file),'_2x2Grid_lc.RData', sep = ''))
# out_file_raster = file.path(out_folder_raster, paste(basename(in_raster_file),'_LAEA', sep = ''))
# out_file_lc_grid = file.path(temp_folder, paste(basename(file_path_sans_ext(in_lc_file)),'rasterized.tif', sep = ''))
# out_file_lc_shape = file.path(temp_folder, paste(basename(file_path_sans_ext(in_lc_file)),'_poly.shp', sep = ''))
# aggregate_lc_RData_file =file.path(out_folder_RData, 'aggregated_lc.RData')
# out_file_raster_aggregated_lc = file.path(out_folder_raster_aggregated, paste(basename(file_path_sans_ext(in_lc_file)),'_LAEA_2x2.tif', sep = ''))
# out_file_raster_aggregated = file.path(out_folder_raster_aggregated, paste(basename(in_raster_file),'_LAEA_2x2', sep = ''))
# #out_file_raster_aggregated_stdev = file.path(out_folder_raster_aggregated_stdev, paste(basename(in_raster_file),'_LAEA_2x2_stdev', sep = ''))

# # create output folders if needed
# dir.create (temp_folder, recursive = T, showWarnings = F)
# dir.create (out_folder_raster, recursive = T, showWarnings = F)
# dir.create (out_folder_raster_aggregated, recursive = T, showWarnings = F)
# #dir.create (out_folder_raster_aggregated_stdev, recursive = T, showWarnings = F)
# dir.create (out_folder_RData, recursive = T, showWarnings = F)

# Define proj4 string for laea projection
# laea_crs = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
# raster_orig_crs = CRS(in_raster_file_proj4)
#
# # Load the ERMES polygon reference Grid (LAEA projection) and convert it to projection of original raster
# # If reprojected grid already existing this is skipped and reprojected grid is read from existing file
# # This avoid redo the transformation for each file to be reprojected/regridded !
#
# out_ermes_grid_reproj = file.path(temp_folder, paste('ERMES_Grid_reprojected',in_raster_proj,'.shp',sep = ''))
# ERMES_cells_poly = readOGR(dirname(in_ermes_grid_laea),file_path_sans_ext(basename(in_ermes_grid_laea)))
#
#
# extent_poly_laea = extent(ERMES_cells_poly)   # Retrieve extnt of ERMES grid in original LAEA proj
# extent_poly_reproj = extent(ERMES_cells_poly_reproj)  # Retrieve extnt of ERMES grid in the projection of input raster
#




#-------------------------------------------- #
#### From here onwards, equal to previous
#-------------------------------------------- #

# Create a temporary raster file with higher spatial resolution than the original, resized on the extent of the ERMES reference grid
temporary_raster = tempfile(fileext =  ".tif")
temp_raster <- gdalwarp(out_temp_raster_masked,temporary_raster, s_srs = laea_crs, t_srs = laea_crs,
         tr = temp_res, r = "near",
         te = c(extent_poly_reproj@xmin, extent_poly_reproj@ymin,extent_poly_reproj@xmax, extent_poly_reproj@ymax),
         overwrite = T, tap = F, output_Raster = T)

# Get the values of the increased resolution input raster
# temp_raster <- raster(file.path(temp_folder, 'temporary_raster.tif'))
data_raster = getValues(temp_raster)
# nodata_pixs = which(data_raster == in_nodata)		# set NODATA to NA
# data_raster [nodata_pixs] = NA

# If not already existing, Rasterize polygons converted to sinusoidal on the extent of phenorice output and at resolution of temporary raster
# this way, I obtain a "dummy" raster coregistered with the temporary input raster. For each pixrel, values of this raster correspond to a
# unique identifier of the cell of the  ERMES grid to which the pixel belongs

if (file.exists(grid_rasterized_laea) == FALSE){
  gdal_rasterize(out_ermes_grid_reproj,grid_rasterized_laea, a = "FID", output_Raster = T,
                 te = c(extent_poly_reproj@xmin, extent_poly_reproj@ymin,extent_poly_reproj@xmax, extent_poly_reproj@ymax),
                 tr = temp_res, tap = F)
}
# Get the values of the "dummy" raster of cells codes
temp_raster_cells <- raster(grid_rasterized_laea)
data_raster_cells = getValues(temp_raster_cells)
data_raster_cells = as.integer(data_raster_cells)

# Create a Data.table with two columns. First column taken from the cell_codes raster - tells to which cell each pixel correspond
# Second column taken from the increased resolution input raster - tells which values correspond to each cell

cells_dt = data.table(int_id = data_raster_cells, value = data_raster)
setkey(cells_dt, int_id)  # Set a indexing key to increase speed of zonal statistics computation
names(cells_dt)[2] = "value"
# Compute the aggregated values for each cell of the ERMES grid - average, stdev, min, max and n° of pixels in each cell
aggregate_values = cells_dt[, list(average = as.numeric(mean(value, na.rm = T)),
                                   stdev = as.numeric(sd(value, na.rm = T)),
                                   min = as.numeric(min(value, na.rm = T)),
                                   max = as.numeric(max(value, na.rm = T)),
                                   n_cells = as.numeric(length(value))
), by = key(cells_dt)]

is.na(aggregate_values) <- do.call(cbind,lapply(aggregate_values, is.infinite))  # Convert Infinite to NA
is.na(aggregate_values) <- do.call(cbind,lapply(aggregate_values, is.nan))
cell_codes = unique(ERMES_cells_poly@data$int_id)
good_codes = 1:length(cell_codes)
aggregate_values = aggregate_values[.(good_codes)] # Remove rows corresponding to areas outside ERMES grid

# Associate the output values to an RData SpatialPointsDataFrame retaining the info originally
# present in the polygon shapefile of cells boundaries + write this Rdata object to a file
# for possible later use

sppoint_out = SpatialPointsDataFrame(coordinates(ERMES_cells_poly), data = as.data.frame(aggregate_values), proj4string = laea_crs, match.ID = F)
sppoint_out@data = join(ERMES_cells_poly@data, sppoint_out@data, by = 'int_id')
save(sppoint_out, file = out_file_RData)

# Write the outputs (aggregated data) of interest as raster files (GTiff or ENVI)
raster_out_mean = rasterFromXYZ(as.data.frame(sppoint_out)[, c("x", "y", "average")], res=c(2000,2000), crs = laea_crs, digits=5)
#raster_out_stdev = rasterFromXYZ(as.data.frame(sppoint_out)[, c("x", "y", "stdev")], res=c(2000,2000), crs = laea_crs, digits=5)
writeRaster(raster_out_mean,out_filename, format = out_format, overwrite = T, NAflag = 255)
#writeRaster(raster_out_stdev,out_filename_stdev, format = out_format, overwrite = T, NAflag = 255)

t2 = Sys.time()
t2-t1


