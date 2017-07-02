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

in_yield = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L4_Yield/2015/Old/IT_EI_L4_Yield_2015.shp'
in_devel = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Local/IT_EI_L5_Development/2015/Old/IT_EI_L5_Development_2015.shp'
out_shapename_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L6_Summary_2015.shp"
out_shapename_yield_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L4_Yield_2015.shp"
out_shapename_dev_or = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/Local/IT_EI_L5_Development_2015.shp"

for (cc in c('it','gr','es')) {

  if (cc == 'it') {
    in_yieldfile = in_yield
    in_develfile = in_devel
    out_shapename = out_shapename_or
    out_shapename_yield = out_shapename_yield_or
    out_shapename_dev = out_shapename_dev_or
  }

  if (cc == 'es') {
    in_yieldfile = gsub("IT","ES",in_yield)
    in_develfile =  gsub("IT","ES",in_devel)
    out_shapename = gsub("IT","ES", out_shapename_or)
    out_shapename_yield = gsub("IT","ES", out_shapename_yield_or)
    out_shapename_dev = gsub("IT","ES", out_shapename_dev_or)
  }
  if(cc == 'gr') {
    in_yieldfile = gsub("IT","GR",in_yield)
    in_develfile =  gsub("IT","GR",in_devel)
    out_shapename = gsub("IT","GR", out_shapename_or)
    out_shapename_yield =  gsub("IT","GR", out_shapename_yield_or)
    out_shapename_dev =  gsub("IT","GR", out_shapename_dev_or)
  }

  in_shp_yield= readshape(in_yieldfile)
  in_yield_data = in_shp_yield@data

  out_yield_data = in_yield_data %>%
    mutate(agb = agb/1000, yield = yield/1000) %>%
    dplyr::select(-gid, -nuts_3,-lau2_nat)

  in_shp_devel= readshape(in_develfile)
  in_devel_data = in_shp_devel@data %>%
    dplyr::select(-gid, -nuts_3,-lau2_nat,-parcel_num, -variety, -name_2_lat)

  out_tot_data = in_devel_data %>%
    mutate(sow_dd = lb_doytodate(sowing_doy,2015),
           emer_dd = lb_doytodate(stagecode_,2015),
           till_dd = lb_doytodate(stagecod_1,2015),
           pan_dd = lb_doytodate(stagecod_2,2015),
           flow_dd = lb_doytodate(stagecod_3,2015),
           mat_dd = lb_doytodate(stagecod_4,2015))

  out_tot_data = out_yield_data %>%
    dplyr::left_join(out_tot_data, by = c('parcel_id')) %>%
    dplyr::select(-(sowing_doy:stagecod_4))

  out_shp = in_shp_yield
  out_shp@data = out_tot_data

  lb_writeshape(out_shp, out_shapename)

  out_yield_mod = dplyr::select(out_tot_data,c(1:7))
  out_dev_mod = dplyr::select(out_tot_data,c(1:5,8:12))

  out_shp@data = out_yield_mod
  lb_writeshape(out_shp, out_shapename_yield)

  out_shp@data = out_dev_mod
  lb_writeshape(out_shp, out_shapename_dev)

}
