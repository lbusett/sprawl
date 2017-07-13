library(data.table)
library(ggplot2)
library(gdalUtils)
library(RPostgreSQL)
# library(sprawl)
library(dplyr)
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(tools)
library(utils)
library(sprawl)

# nrworking_dir <- 'W:'
nrworking_dir <- '//10.0.1.252/nr_working'
# ermes_dir <- 'V:'
ermes_dir <- '//10.0.1.252/projects/ermes'

cc = 'es'

# open the DB
source(file.path(nrworking_dir,'luigi/code/Ermes/WARM_DB/opencon.R'))

#define input rasters

for (cc in c('gr','es','it')) {

  in_rast_ricemap_dir = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R1_Rice_Map/2015/Full/'
  in_rast_sow_dir = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R2_Phenology/2015/MinDoys/'
  in_rast_flw_dir = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R2_Phenology/2015/MaxDoys/'
  out_filename_in =  "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Regional/IT_Statistics_Shapefile_2015.shp"

  if (cc == 'it') {
    in_rast_ricemap = list.files(in_rast_ricemap_dir, "\\.tif$", full.names = T)
    in_rast_sow = list.files(in_rast_sow_dir, "\\.tif$", full.names = T)
    in_rast_flw = list.files(in_rast_flw_dir, "\\.tif$", full.names = T)
    out_filename = out_filename_in
  }

  if (cc == 'es') {
    in_rast_ricemap = list.files(gsub("IT","ES",in_rast_ricemap_dir), "\\.tif$", full.names = T)
    in_rast_sow = list.files(gsub("IT","ES",in_rast_sow_dir), "\\.tif$", full.names = T)
    in_rast_flw = list.files(gsub("IT","ES",in_rast_flw_dir), "\\.tif$", full.names = T)
    out_filename = gsub("IT","ES",out_filename_in)
  }
  if(cc == 'gr') {
    in_rast_ricemap = list.files(gsub("IT","GR",in_rast_ricemap_dir), "\\.tif$", full.names = T)
    in_rast_sow = list.files(gsub("IT","GR",in_rast_sow_dir), "\\.tif$", full.names = T)
    in_rast_flw = list.files(gsub("IT","GR",in_rast_flw_dir), "\\.tif$", full.names = T)
    out_filename = sub("IT","GR",out_filename_in)
  }

  # Define schema and layer name of administrative areas layer

  schema_name = paste0("reg_",cc)
  table_name = paste0("administrative_areas")

  # Get the administrative areas layer

  con <- dbConnect(dbDriver("PostgreSQL"), user=LB_WDB_opts$db_user,password=LB_WDB_opts$db_pwd,
                   dbname=LB_WDB_opts$db_name, host=LB_WDB_opts$db_address, port = LB_WDB_opts$db_port)


  in_shp = lb_get_PGIS(schema_name, table_name, 'geom', con, verbose = T)
  if (cc == 'it') in_shp = subset(in_shp, type == "municip")
  if (cc == 'es') in_shp = subset(in_shp, type == "municipality")
  if (cc == 'gr') in_shp = subset(in_shp, type == "macroarea")
  # in_rast_map = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/IT_Ricemap_Full_2015_254.tif"
  in_rast_ricemap = raster(in_rast_ricemap)
  in_rast_flw = raster(in_rast_flw)
  in_rast_sow = raster(in_rast_sow)

  in_rast_area = setZ(stack(in_rast_ricemap), as.Date('2015-12-31'), name = 'time')
  NAvalue(in_rast_area) = 255
  area = MODIStsp_extract(in_rast_area, in_shp, id_field = "adm_id", out_format = 'dframe', FUN = sum, verbose = T)
  area[2:length(area)] = area[2:length(area)]*20*20/10000
  area = melt(area, id.vars = 'date')
  names(area)[2:3] = c('adm_id',"RiceAreaha")


  fc = MODIStsp_extract(in_rast_area, in_shp, id_field = "adm_id", out_format = 'dframe', FUN = mean, verbose = T)
  fc[2:length(fc)] = fc[2:length(fc)]*100
  fc = melt(fc, id.vars = 'date')
  names(fc)[2:3] = c('adm_id',"RiceFC")

  in_rast_sow = setZ(stack(in_rast_sow), as.Date('2015-12-31'), name = 'time')
  avgsow = MODIStsp_extract(in_rast_sow, in_shp, id_field = "adm_id", out_format = 'dframe', verbose = T)
  avgsow = melt(avgsow, id.vars = 'date')
  names(avgsow)[2:3] = c('adm_id','avgsow')

  in_rast_flw = setZ(stack(in_rast_flw), as.Date('2015-12-31'), name = 'time')
  avgflw = MODIStsp_extract(in_rast_flw, in_shp, id_field = "adm_id", out_format = 'dframe', verbose = T)
  avgflw = melt(avgflw, id.vars = 'date')
  names(avgflw)[2:3] = c('adm_id','avgflw')

  if (cc == "es") {

    out = in_shp@data %>%
      left_join(area) %>%
      left_join(fc) %>%
      left_join(avgsow) %>%
      mutate(avgsow = lb_doytodate(avgsow, 2015)) %>%
      left_join(avgflw) %>%
      mutate(avgflw = lb_doytodate(avgflw, 2015)) %>%
      mutate(yield = c(rep(6.1,6),rep(7.2, 4), 6.1,6.1,7.2), n_risk = c(rep(10,6), rep(13,4), 10,10,13)) %>%
      dplyr::select(-date, -gid, -type, -nuts_2,-nuts_3, -macroarea, -shape_area)
  }

  if (cc == "gr") {

    out = in_shp@data %>%
      left_join(area) %>%
      left_join(fc) %>%
      left_join(avgsow) %>%
      mutate(avgsow = lb_doytodate(avgsow, 2015)) %>%
      left_join(avgflw) %>%
      mutate(avgflw = lb_doytodate(avgflw, 2015)) %>%
      mutate(yield = c(6.28,9.43), n_risk = NA) %>%
      dplyr::select(-date, -gid, -type, -nuts_2,-nuts_3, -macroarea, -shape_area)
  }


  if (cc == "it") {

    out = in_shp@data %>%
      left_join(area) %>%
      left_join(fc) %>%
      left_join(avgsow) %>%
      mutate(avgsow = lb_doytodate(avgsow, 2015)) %>%
      left_join(avgflw) %>%
      mutate(avgflw = lb_doytodate(avgflw, 2015)) %>%
      mutate(yield = NA, n_risk = NA) %>%
      dplyr::select(-date, -gid, -type, -nuts_2,-nuts_3, -macroarea, -shape_area)
  }

  out_shp = in_shp
  out_shp@data = out
  proj4string(out_shp) = "+init=epsg:3035"

  write_shape(out_shp,out_filename)

}
