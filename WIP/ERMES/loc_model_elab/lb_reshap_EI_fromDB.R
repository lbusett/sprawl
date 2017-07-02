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

table_yield = 'location_yield_2015'
table_stage = 'location_stagecode_2015'
table_manag = 'location_management_2015'
out_dir_base = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/New"
dir.create(out_dir_base, recursive = T)
in_devel = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L5_Development/2015/Old/IT_EI_L5_Development_2015.shp'
out_shapename_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L6_Summary_2015.shp"
out_shapename_yield_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L4_Yield_2015.shp"
out_shapename_dev_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L5_Development_2015.shp"



for (cc in c('it','gr','es')) {

  if (cc == 'it') {
    in_schema = "loc_it"
    out_shapename = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L6_Summary/2015/IT_EI_L6_Summary_2015.shp")
    out_shapename_yield = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L4_Yield/2015/IT_EI_L4_Yield_2015.shp")
    out_shapename_dev = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L5_Development/2015/IT_EI_L5_Development_2015.shp")

  }

  if (cc == 'es') {
    in_schema = "loc_es"
    out_shapename = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/ES/Local/ES_EI_L6_Summary/2015/ES_EI_L6_Summary_2015.shp")
    out_shapename_yield = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/ES/Local/ES_EI_L4_Yield/2015/ES_EI_L4_Yield_2015.shp")
    out_shapename_dev = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/ES/Local/ES_EI_L5_Development/2015/ES_EI_L5_Development_2015.shp")
  }
  if(cc == 'gr') {
    in_schema = "loc_gr"
    out_shapename = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/GR/Local/GR_EI_L6_Summary/2015/GR_EI_L6_Summary_2015.shp")
    out_shapename_yield = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/GR/Local/GR_EI_L4_Yield/2015/GR_EI_L4_Yield_2015.shp")
    out_shapename_dev = file.path("Y:/ermes/datasets/ERMES_Folder_Structure/GR/Local/GR_EI_L5_Development/2015/GR_EI_L5_Development_2015.shp")
  }

  # dir.create(dirname(out_shapename), recursive = T)
  # dir.create(dirname(out_shapename_yield), recursive = T)
  # dir.create(dirname(out_shapename_dev), recursive = T)

  # in_shp_yield= lb_get_PGIS(in_yieldfile,table_yield, in_schema, con )
  in_yield_data = lb_get_PGIS(in_schema ,table_yield, con, geom_field = "geom" )
  in_yield_manag = lb_get_PGIS(in_schema ,table_manag, con, geom_field = "geom" )
  out_yield_data = in_yield_data@data %>%
    mutate(agb = agb/1000, yield = yield/1000) %>%
    dplyr::select(-gid, -nuts_3,-lau2_nat)

  in_devel_data = lb_get_PGIS(in_schema,table_stage, con, geom_field = "geom" )
  in_devel_data = in_devel_data@data %>%
    dplyr::select(-gid, -nuts_3,-lau2_nat,-parcel_num, -variety, -name_2_lat)

  out_tot_data = in_devel_data %>%
    mutate(sow_dd = lb_doytodate(sowing_doy,2015),
           emer_dd = lb_doytodate(stagecode_10_doy,2015),
           till_dd = lb_doytodate(stagecode_13_doy,2015),
           pan_dd = lb_doytodate(stagecode_16_doy,2015),
           flow_dd = lb_doytodate(stagecode_20_doy,2015),
           mat_dd = lb_doytodate(stagecode_30_doy,2015))

  in_yield_manag = dplyr::select(in_yield_manag@data, parcel_id, variety)

  out_tot_data = out_yield_data %>%

    dplyr::left_join(out_tot_data, by = c('parcel_id')) %>%
    dplyr::select(-(sowing_doy:stagecode_30_doy)) %>%
    dplyr::left_join(in_yield_manag, by = c('parcel_id')) %>%
    dplyr::mutate(variety.x = variety.y)

  out_tot_data = dplyr::select(out_tot_data, -variety.y)

  out_shp = in_yield_data
  out_shp@data = out_tot_data

  lb_writeshape(out_shp, out_shapename)

  out_yield_mod = dplyr::select(out_tot_data,c(1:7))
  out_dev_mod = dplyr::select(out_tot_data,c(1:5,8:12))

  out_shp@data = out_yield_mod
  lb_writeshape(out_shp, out_shapename_yield)

  out_shp@data = out_dev_mod
  lb_writeshape(out_shp, out_shapename_dev)

}

dbDisconnect(con)
