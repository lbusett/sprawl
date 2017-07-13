# library(data.table)
# library(ggplot2)
# library(gdalUtils)
# library(RPostgreSQL)
# # library(sprawl)
# library(dplyr)
# library(rgdal)
# library(sp)
# library(rgeos)
# library(raster)
# library(tools)
# library(utils)
# library(sprawl)

lb_compute_pheno_stats_fiveyears = function(cc,in_rast_ricemap = NULL,in_rast_flw = NULL, in_rast_sow = NULL , out_folder = NULL, out_shapefile = out_shapefile, sow = NULL, flw = NULL, area = NULL , con = con){

  # Get the administrative areas layer
  in_shp_saved = file.path(dirname(out_folder),"ancillary",cc,"in_admin.shp")
  dir.create(dirname(in_shp_saved), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(in_shp_saved) == TRUE) {
    in_shp = readshape(in_shp_saved)
  } else {
    in_shp = lb_get_PGIS(schema_name, table_name, geom_field = 'geom', con, verbose = T)
    write_shape(in_shp, filename = in_shp_saved )
  }

  if (cc == 'it') in_shp = subset(in_shp, type == "municip")
  if (cc == 'es') in_shp = subset(in_shp, type == "municipality")
  if (cc == 'gr') in_shp = subset(in_shp, type == "macroarea")
  # in_rast_map = "D:/Documents/ERMES/Documents/Data_Processing/FOR_Stats_Shapefiles/IT_Ricemap_Full_2015_254.tif"
  # in_rast_ricemap = raster(in_rast_ricemap)
  # in_rast_flw = raster(in_rast_flw)
  # in_rast_sow = raster(in_rast_sow)

  in_rast_area = setZ(stack(in_rast_ricemap), as.Date('2015-12-31'), name = 'time')
  if (cc == 'it') {
    in_rast_area[in_rast_area == 2] = 0
    in_rast_area = setZ(stack(in_rast_area), as.Date('2015-12-31'), name = 'time')
  } else {
    NAvalue(in_rast_area) = 255
  }

  message("Computing Area")

  area = lb_fastzonal(in_rast_area, in_shp, id_field = "adm_id", out_format = 'dframe', FUN = sum, verbose = F)
  area[2:length(area)] = area[2:length(area)]*res(in_rast_area)[1]*res(in_rast_area)[2]/10000
  area = melt(area, id.vars = 'date')
  names(area)[2:3] = c('adm_id',"RiceAreaha")

  message("Computing fc")
  fc = lb_fastzonal(in_rast_area, in_shp, id_field = "adm_id", out_format = 'dframe', FUN = mean, verbose = F)
  fc[2:length(fc)] = fc[2:length(fc)]*100
  fc = melt(fc, id.vars = 'date')
  names(fc)[2:3] = c('adm_id',"RiceFC")

  message("Computing sow")
  # in_rast_sow = setZ(stack(in_rast_sow), as.Date('2015-12-31'), name = 'time')
  avgsow = lb_fastzonal(in_rast_sow, in_shp, id_field = "adm_id", out_format = 'dframe', verbose = F, start_date = as.Date("2011-01-01"), end_date =
                          as.Date("2016-01-01"))
  avgsow = melt(avgsow, id.vars = 'date')
  names(avgsow)[2:3] = c('adm_id','avgsow')

  if (is.null(sow)) {
    avgsow$avgsow = NA
  }
  message("Computing flw")
  # in_rast_flw = setZ(stack(in_rast_flw), as.Date('2015-12-31'), name = 'time')
  avgflw = lb_fastzonal(in_rast_flw, in_shp, id_field = "adm_id", out_format = 'dframe', verbose = F, start_date = as.Date("2011-01-01"), end_date =
                          as.Date("2016-01-01"))
  avgflw = melt(avgflw, id.vars = 'date')
  names(avgflw)[2:3] = c('adm_id','avgflw')

  if (is.null(flw)) {
    avgflw$avgflw = NA
  }


  # if (cc == "es") {
  #
  # 	out = in_shp@data %>%
  # 		left_join(area) %>%
  # 		left_join(fc) %>%
  # 		left_join(avgsow) %>%
  # 		mutate(avgsow = lb_doytodate(avgsow, 2015)) %>%
  # 		left_join(avgflw) %>%
  # 		mutate(avgflw = lb_doytodate(avgflw, 2015)) %>%
  # 		mutate(yield = NA) %>%
  # 		dplyr::select(-gid, -type, -nuts_2,-nuts_3, -macroarea, -shape_area)
  # }

  # if (cc == "gr") {

  out = avgsow  %>%
    left_join(avgflw)  %>%
    right_join(area, by = "adm_id")  %>%
    right_join(fc, by = "adm_id")  %>%
    left_join(in_shp@data) %>%
    mutate(year = year(date.x))%>%
    mutate(avgsow_date = lb_doytodate(avgsow, year)) %>%
    mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
    mutate(anomsow = avgsow - mean(avgsow[date != as.Date("2016-01-01")], na.rm = TRUE)) %>%
    mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
    mutate(anomflw = avgflw - mean(avgflw[date != as.Date("2016-01-01")], na.rm = TRUE)) %>%
    mutate(yield = NA, n_risk = NA) %>%
    mutate(gid = seq(1, length(adm_id),1)) %>%
    dplyr::select(-date.x, -date.y, -date, -shape_area)%>%
    select(gid, adm_id, label,nuts_2, nuts_3, type, year, RiceAreaha, RiceFC,avgsow, avgsow_date,anomsow, avgflw,avgflw_date, anomflw,yield, n_risk)

  # out = avgsow  %>%
  #   left_join(avgflw)  %>%
  #   right_join(area, by = "adm_id")  %>%
  #   right_join(fc, by = "adm_id")  %>%
  #   left_join(in_shp@data) %>%
  #   mutate(year = year(date.x))%>%
  #   mutate(avgsow_date = lb_doytodate(avgsow, year)) %>%
  #   mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
  #   mutate(anomsow = avgsow - mean(avgsow[year != as.Date("2012-01-01")], na.rm = TRUE)) %>%
  #   mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
  #   mutate(anomflw = avgflw - mean(avgsow[year != as.Date("2012-01-01")], na.rm = TRUE)) %>%
  #   mutate(yield = NA, n_risk = NA) %>%
  #   mutate(gid = seq(1, length(adm_id),1)) %>%
  #   dplyr::select(-date.x, -date.y, -date, -shape_area)%>%
  #   select(gid, adm_id, label,nuts_2, nuts_3, type, year, RiceAreaha, RiceFC,avgsow, avgsow_date,anomsow, avgflw,avgflw_date, anomflw,yield, n_risk)
  #
  #
  # out = avgsow %>%
  #   left_join(avgflw)  %>%
  #   mutate(year = year(date))%>%
  #   mutate(avgsow_date = lb_doytodate(avgsow, year)) %>%
  #   mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
  #   mutate(anomsow = avgsow - mean(avgsow[date != as.Date("2016-01-01")], na.rm = TRUE)) %>%
  #   mutate(avgflw_date = lb_doytodate(avgflw, year)) %>%
  #   mutate(anomflw = avgflw - mean(avgflw[date != as.Date("2016-01-01")], na.rm = TRUE)) %>%
  #   mutate(yield = NA, n_risk = NA) %>%
  #   mutate(gid = seq(1, length(adm_id),1)) %>%
  #   dplyr::select(-date.x, -date.y, -date, -shape_area)%>%
  #   select(gid, adm_id, label,nuts_2, nuts_3, type, year, RiceAreaha, RiceFC,avgsow, avgsow_date,anomsow, avgflw,avgflw_date, anomflw,yield, n_risk)
  #


  polys = list()
  for(row in seq(along = out$adm_id)) {

    polytemp = polygons(in_shp)@polygons[which(in_shp@data$adm_id == out$adm_id[row])]
    polytemp[[1]]@ID = as.character(row)
    polys[[row]] = polytemp[[1]]

  }

  polynew = SpatialPolygons(polys, proj4string = CRS(proj4string(in_shp)))
  out_shp= SpatialPolygonsDataFrame(polynew, out)
  write_shape(out_shp,out_shapefile)

  return(out_shp@data)

}

