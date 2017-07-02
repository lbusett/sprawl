# Setting inputs and outputs --------  > Set to "MOD" or "VGT"


main_folder =  "/home/lb/Documents/aggregate_for_local"
script_folder = "/home/lb/Documents/aggregate_for_local/source_code/"
countries <- c("it","es","gr")
types <- c("MOD","VGT")
fc_threshold = 0.75  # set the arable land fc threshold

# Intializing ----
args <- commandArgs(TRUE)
t1 = Sys.time()
args
# load required libraries (install if missing)
pkg_list = c('tools','raster','sp', 'gdalUtils','rgdal','data.table','plyr', 'utils', 'xts')
pkg_test <- function(x) {
  if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)
    require(x,character.only=TRUE)}
}
for (pkg in pkg_list) {pkg_test(pkg)}
memory.limit(8000)
rasterOptions (setfileext = F)

# Load scripts

source(file.path(script_folder,"lb_fastzonal.R" ))
source(file.path(script_folder,"readshape" ))
source(file.path(script_folder,"lb_writeshape.R" ))


# Processing ----
# Start cycling on coiuntries and type of coarse resolution LAI
for(type in types) {

  for (country in countries) {

    in_folder           = file.path(main_folder, country, 'in_rasters')
    count_folder        = file.path(main_folder,country)
    coarse_file         = ifelse (type == "MOD",  file.path(count_folder,"template_rasters", "MOD_template"),file.path(count_folder,"template_rasters", "VGT_template"))
    templ_2k_file       = file.path(count_folder,"template_rasters", "2k_template.tif")
    in_lc_file          = file.path(count_folder,"template_rasters", "lc_map.tif")
    in_ermes_grid_laea  = file.path(count_folder,"ermes_grid",  "grid_laea.shp")
    in_nodata = -1   # set to nodata value of input LAI images

    out_folder = file.path(count_folder, 'out_aggr_LAI',type)  # set to output folder
    dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)

    # start -----------

    # get list of tiff filenames to be processed

    in_list = list.files(in_folder, pattern = '.HDR$', full.names = TRUE)
    # Start cycling on input files

    for (file in in_list) {
      message("Processing File: ", file, "- Aggregating to 2Km grid as if it was a ",type," file")
      # set output filename and format
      infile = file_path_sans_ext(file)
      out_filename_avg = file.path(out_folder,paste0(basename(infile),'avg_2km.tif'))
      out_filename_sd = file.path(out_folder,paste0(basename(infile),'sd_2km.tif'))

      # get projections of in and lc file
      infile_proj4 = proj4string(raster(infile))
      in_lc_raster_proj = proj4string(raster(in_lc_file))
      laea_crs = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

      out_folder_raster = file.path(out_folder,'Reprojected_Native_Res')
      out_file_raster = file.path(out_folder_raster, paste(basename(infile),'_LAEA', sep = ''))
      out_ermes_grid_reproj = tempfile(fileext = '.shp')

      # Reproject grid to input file projection
      ERMES_cells_poly = readshape(in_ermes_grid_laea)
      ERMES_cells_poly_reproj = spTransform(ERMES_cells_poly, infile_proj4)
      lb_writeshape(ERMES_cells_poly_reproj,out_ermes_grid_reproj)
      extent_poly_reproj = extent(ERMES_cells_poly_reproj)  # Retrieve extnt of ERMES grid in the projection of input raster
      # extent_poly_laea = extent(ERMES_cells_poly)           # Retrieve extnt of ERMES grid in original LAEA proj

      # crop ermes grid to input file extent

      fullgrid = readshape(out_ermes_grid_reproj)
      extout = extent(raster(infile))
      cropped_grid = crop(fullgrid,extout+6000)

      ext_cropped_laea = extent(spTransform(cropped_grid, laea_crs))
      cropped_grid_LAEA = spTransform(cropped_grid, laea_crs)
      extout_laea_initial = ext_cropped_laea-6000
      temprast_LAEA = tempfile(fileext = '.tif')
      # Reproject the original raster to LAEA projection , clip it on ERMES cropped Grid extent
      gdalwarp(infile,temprast_LAEA, s_srs = infile_proj4, t_srs = laea_crs,
               r = "near", te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
               srcnodata = in_nodata, dstnodata =  in_nodata, of = 'GTiff', overwrite = T)

      # Compute fc arable on pixels of original coarse resoultion file (MOD or VGT)
      # then create a mask to be applied on the input HR lai file
      in_lc_file_temp = tempfile(fileext = '.tif')
      lc_temp = gdalwarp(in_lc_file, in_lc_file_temp, s_srs = laea_crs, t_srs = laea_crs, output_Raster = T,
                         te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
                         r = "near", srcnodata = 255, dstnodata =  255, of = "GTiff", overwrite = T, tr = res(raster(temprast_LAEA)) )

      # to Find out shich pixels of the original LAI file were NOT used in the aggregation of the coarase resolution file to 2km
      # open an example coarse res file to build a grid on to which computing the fcs
      tempfile_coarse_repr = tempfile(fileext = '.tif')
      coarse_repr = gdalwarp(coarse_file,tempfile_coarse_repr, s_srs = proj4string(raster(coarse_file)), t_srs = laea_crs, output_Raster = TRUE, overwrite = T,
                             te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax))
      rtemp_repr = crop(raster(tempfile_coarse_repr),ext_cropped_laea)
      grid = getGridTopology(as(coarse_repr, "SpatialGrid"))		# create a grid from the cropped coarse res file
      pix_grid = SpatialGrid(grid,laea_crs)
      pix_grid_cell = SpatialGridDataFrame(pix_grid, data = data.frame(id = seq(1:(dim(rtemp_repr)[1]*dim(rtemp_repr)[2]))),laea_crs)
      sp_polygons_fc = as(pix_grid_cell, "SpatialPolygonsDataFrame")	#create the shape - convert grid to polygons
      extent_poly_fc = extent(sp_polygons_fc)
      #   # write to shapefile
      temp_grid_coarse = tempfile(fileext = '.shp')
      lb_writeshape(sp_polygons_fc,temp_grid_coarse)   # write temporarty grid

      # Compute the fcs
      indata = stack(crop(raster(in_lc_file), ext_cropped_laea))
      indata = setZ(indata, as.Date('2015-01-01'), name = "time")
      arable_fc  = lb_fastzonal(indata, sp_polygons_fc,id_field = "id", verbose = F, out_format = 'xts', small = T)
      arable_fc_df = data.frame(id = as.numeric(names(arable_fc)), arab_fc = as.numeric(arable_fc))
      sp_polygons_fc@data = join(sp_polygons_fc@data, arable_fc_df, type = 'left')
      lb_writeshape(sp_polygons_fc, temp_grid_coarse)
      temprast_fc = tempfile(fileext = '.tif')
      file.copy(temprast_LAEA,temprast_fc)
      gdal_rasterize(temp_grid_coarse, temprast_fc, a = "arab_fc")
      temprast_fc_repro = tempfile(fileext = '.tif')
      extout_laea = extent(raster(temprast_LAEA))
      gdalwarp(temprast_fc,temprast_fc_repro, s_srs = laea_crs, t_srs = laea_crs,
               te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
               r = "near", srcnodata = 255, dstnodata =  255, of = "GTiff", overwrite = T, tr = res(raster(temprast_LAEA)))
      mask_fc = raster(temprast_fc_repro)
      mask_fc[mask_fc >= fc_threshold] = 1
      mask_fc[mask_fc < fc_threshold] = 0

      masked_LAI_map = mask(raster(temprast_LAEA), mask_fc,maskvalue = 0)
      masked_LAI_map [masked_LAI_map == 0 ] = NA

      # tempraster_masked = tempfile(fileext = '.tif')
      # out_temp_raster_masked = tempfile(fileext = '.tif')
      # writeRaster(masked_LAI_map ,filename = tempraster_masked, overwrite = T)
      #
      # Extract avg and sd LAI values from the masked image ----
      indata = stack(masked_LAI_map)
      indata = setZ(indata, as.Date('2015-01-01'), name = "time")
      avg_LAI  = lb_fastzonal(indata, cropped_grid_LAEA,id_field = "int_id", verbose = F, out_format = 'xts', na.rm = TRUE)
      avg_LAI_df = data.frame(int_id = as.numeric(names(avg_LAI)), LAIavg = as.numeric(avg_LAI))

      sd_LAI  = lb_fastzonal(indata, cropped_grid_LAEA,id_field = "int_id", verbose = F, out_format = 'xts', FUN = sd, na.rm = T)
      sd_LAI_df = data.frame(int_id = as.numeric(names(sd_LAI)), LAIsd = as.numeric(sd_LAI))

      coarselai_shape = cropped_grid_LAEA
      newdata = join(coarselai_shape@data, avg_LAI_df, type = 'left')
      newdata = join(newdata, sd_LAI_df, type = 'left')
      coarselai_shape@data = newdata
      tempshpfile = tempfile(fileext = '.shp')
      lb_writeshape(coarselai_shape, tempshpfile)
      # browser()
      in_2k = crop(raster(templ_2k_file), extent(cropped_grid_LAEA))

      temp2k_file = tempfile(fileext = '.tif')
      writeRaster(in_2k, temp2k_file)
      temprast = tempfile(fileext = '.tif')
      temprastsd = tempfile(fileext = '.tif')
      file.copy(temp2k_file,temprast)
      file.copy(temp2k_file,temprastsd)
      out_rast = crop(gdal_rasterize(tempshpfile, temprast, a ="LAIavg", output_Raster = TRUE),extout_laea_initial)
      out_rast_sd = crop(gdal_rasterize(tempshpfile, temprastsd, a ="LAIsd", output_Raster = TRUE),extout_laea_initial)
      out_rast_sd[out_rast_sd == 0] = -1
      # raster(temprast)
      writeRaster(out_rast, out_filename_avg, NAflag = -1, overwrite = T)
      writeRaster(out_rast_sd, out_filename_sd, NAflag = -1, overwrite = T)
    }

  }

}



