
# Script to plot output profiles of some Greek parcels

library(data.table)
library(ggplot2)
library(gdalUtils)

# nrworking_dir <- 'W:'
nrworking_dir <- '//10.0.1.252/nr_working'
# ermes_dir <- 'V:'
ermes_dir <- '//10.0.1.252/projects/ermes'

cc = 'es'

# open the DB
source(file.path(nrworking_dir,'luigi/code/Ermes/WARM_DB/opencon.R'))

# loc_data <- data.table(dbGetQuery(con,paste0('SELECT parcel_num, parcels.parcel_id, year, doy, infections, stagecode, ground_biomass, org_biomass ',
#                                              'FROM loc_',cc,'.out_warm ',
#                                              'LEFT JOIN loc_',cc,'.simulation_units ON simulation_units.simulation_id = out_warm.simulation_id ',
#                                              'LEFT JOIN loc_',cc,'.parcels ON simulation_units.parcel_id = parcels.parcel_id ',
#                                              'AND unit_type = \'parcel\'')))
# loc_data$Date15 <- as.POSIXct(paste(loc_data$year,loc_data$doy),format='%Y %j')

loc_data = data.table(dbGetQuery(con,paste0('SELECT parcel_id, year, doy, infections, pot_infections, stagecode, ground_biomass, org_biomass FROM loc_', cc,'.out_warm ')))

# raw_data <- data.table(dbGetQuery(con,paste0('SELECT int_id, year, doy, infections, cum_infections, pot_infections, stagecode FROM reg_',cc,'.temp_out_comp_rh_archive')))
# raw_data$Date15 <- as.POSIXct(paste(raw_data$year,raw_data$doy),format='%Y %j')
# raw_data <- raw_data[!is.na(int_id),]

            # D:\Temp\GIS_for_thessaloniki\GIS_for_thessaloniki\Layers\IT\IT_EI_L4_Yield\2015
shp_file = 'Y:/ermes/datasets/ERMES_Folder_Structure/ES/Local/ES_EI_L4_Yield/2015/ES_EI_L4_Yield_2015.shp'
shp = readshape(shp_file)
# basename = basename(file_path_sans_ext(shp_file))
# dirname = dirname(shp_file)
# shp = readOGR(dirname, basename)

loc_data = subset(loc_data, year == 2015)
library(doParallel)
registerDoParallel(cores=2)
# foreach(i=1:3) %dopar% sqrt(i)

# foreach(dd = c(112,208), .packages = c('raster','plyr', 'rgdal','gdalUtils')) %dopar% {

for(dd in 92:274) {
  print(dd)

  yy = loc_data$year[dd]
  doy = loc_data$doy[dd]
  data_dd = subset(loc_data, year  == yy & doy == dd)
  shp_temp = shp
  shp_temp@data = join(shp_temp@data,data_dd, type = 'left' )
  tmp_shpfile = "D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/tmp_shpfile.shp"
  dir.create(dirname(tmp_shpfile))

  temp_shp = writeOGR(shp_temp,dsn = "D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp",  layer = "tmp_shpfile", driver = "ESRI Shapefile", overwrite_layer = T)
  gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/inf/","temp_inf_2015_",dd,".tiff"), a = "pt_nfct", tr = c(20,20))
  gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/stage/","temp_stage_2015_",dd,".tiff"), a = "stagecd", tr = c(20,20))
  gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/grnd/","temp_grnd_2015_",dd,".tiff"), a = "grnd_bm", tr = c(20,20))
  gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/org/","temp_org_2015_",dd,".tiff"), a = "org_bms", tr = c(20,20))
  # if (dd == 92) {
  #   stack_inf = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_inf_2015_92.tiff")
  #   stack_stage = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_stage_2015_92.tiff")
  #   stack_grnd = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_grnd_2015_92.tiff")
  #   stack_org = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_org_2015_92.tiff")
  # } else {
  #   stack_inf = stack(stack_inf,raster("F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff"))
  #   stack_stage = stack(stack_stage,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
  #   stack_grnd = stack(stack_grnd,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
  #   stack_org = stack(stack_org,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
  #
  # }

}



# for (dd in seq(92,274)) {
#   print(dd)
#
#   yy = loc_data$year[dd]
#   doy = loc_data$doy[dd]
#   data_dd = subset(loc_data, year  == yy & doy == dd)
#   shp_temp = shp
#   shp_temp@data = join(shp_temp@data,data_dd, type = 'left' )
#   tmp_shpfile = "D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/tmp_shpfile.shp"
#   dir.create(dirname(tmp_shpfile))
#
#   temp_shp = writeOGR(shp_temp,dsn = "D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp",  layer = "tmp_shpfile", driver = "ESRI Shapefile", overwrite_layer = T)
#   gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/inf/","temp_inf_2015_",dd,".tiff"), a = "infections", tr = c(10,10))
#   gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/stage/","temp_stage_2015_",dd,".tiff"), a = "stagecode", tr = c(10,10))
#   gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/grnd/","temp_grnd_2015_",dd,".tiff"), a = "agb", tr = c(10,10))
#   gdal_rasterize(tmp_shpfile,paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/org/","temp_org_2015_",dd,".tiff"), a = "yield", tr = c(10,10))
#   # if (dd == 92) {
#   #   stack_inf = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_inf_2015_92.tiff")
#   #   stack_stage = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_stage_2015_92.tiff")
#   #   stack_grnd = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_grnd_2015_92.tiff")
#   #   stack_org = raster("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/IT/temp/temp_org_2015_92.tiff")
#   # } else {
#   #   stack_inf = stack(stack_inf,raster("F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff"))
#   #   stack_stage = stack(stack_stage,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
#   #   stack_grnd = stack(stack_grnd,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
#   #   stack_org = stack(stack_org,"F:/GIS_for_thessaloniki/Layers/temp/temp_inf_2015.tiff")
#   #
#   # }
#
# }

for (dd in  seq(1,92)) {
  print(dd)
  file.copy("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/inf/temp_inf_2015_93.tiff",paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/inf/temp_inf_2015_",dd,".tiff"), overwrite = T)
  file.copy("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/grnd/temp_grnd_2015_93.tiff",paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/grnd/temp_grnd_2015_",dd,".tiff"), overwrite = T)
  file.copy("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/org/temp_org_2015_93.tiff",paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/org/temp_org_2015_",dd,".tiff"), overwrite = T)
  file.copy("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/stage/temp_stage_2015_93.tiff",paste0("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/stage/temp_stage_2015_",dd,".tiff"), overwrite = T)
  }


# save(stack_inf,stack_stage, stack_grnd, stack_org, file = 'D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/IT/temp/temprdata.RData')

list_stage = list.files("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/stage", pattern = '*.tiff$', full.names = T)
doystring_ASC_VV = as.numeric(tstrsplit(file_path_sans_ext(basename(list_stage)),'_')[[4]])
list_stage = list_stage[order(doystring_ASC_VV)]
list_date = lb_doytodate(doystring_ASC_VV[order(doystring_ASC_VV)],2015)
a = stack(list_stage)
lb_writeenvits(a ,list_date ,"D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/stage/envi_stage")
#
# writeRaster(a, filename = "D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/temp/stage/envi_stage",overwrite = T, format = 'ENVI')
# bandnames = paste(basename(file_path_sans_ext(out_file)),in_dates, sep = '_')
# wl = as.numeric(lb_datetodoy(in_dates)+365*(year(in_dates)-min(year(in_dates))))
# hdrfile = paste0(file_path_sans_ext(out_file),'.hdr')
# write(paste("Band Names = {", paste (bandnames, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)ord
# write(paste("wavelength = {", paste (wl, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)

list_stage = list.files("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/grnd", pattern = '*.tiff$', full.names = T)
doystring_ASC_VV = as.numeric(tstrsplit(file_path_sans_ext(basename(list_stage)),'_')[[4]])
list_stage = list_stage[order(doystring_ASC_VV)]
list_date = lb_doytodate(doystring_ASC_VV[order(doystring_ASC_VV)],2015)
a = stack(list_stage)
lb_writeenvits(a ,list_date ,"D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/grnd/envi_grnd")

list_stage = list.files("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/org", pattern = '*.tiff$', full.names = T)
doystring_ASC_VV = as.numeric(tstrsplit(file_path_sans_ext(basename(list_stage)),'_')[[4]])
list_stage = list_stage[order(doystring_ASC_VV)]
list_date = lb_doytodate(doystring_ASC_VV[order(doystring_ASC_VV)],2015)
a = stack(list_stage)
lb_writeenvits(a ,list_date ,"D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/org/envi_org")

list_stage = list.files("D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/inf", pattern = '*.tiff$', full.names = T)
doystring_ASC_VV = as.numeric(tstrsplit(file_path_sans_ext(basename(list_stage)),'_')[[4]])
list_stage = list_stage[order(doystring_ASC_VV)]
list_date = lb_doytodate(doystring_ASC_VV[order(doystring_ASC_VV)],2015)
a = stack(list_stage)
lb_writeenvits(a ,list_date ,"D:/Temp/GIS_for_thessaloniki/GIS_for_thessaloniki/Layers/ES/temp/inf/envi_inf")


#
#
# year_asc = strtrim(doystring_ASC_VV,4)
# day_asc = substr(doystring_ASC_VV,7,9)
# month_asc = substr(doystring_ASC_VV,5,6)
# date_asc_VV = as.Date(paste(year_asc,month_asc,day_asc,sep ='-'))
#
# pro = writeRaster(stack_stage,filename = 'F:/GIS_for_thessaloniki/Layers/temp/stack_stage.tiff', datatype = 'FLT4S' )
#
# pro = join(raw_data, shp_file )
#
# rgaloc_data_long <- melt(loc_data, id.vars = c('parcel_num','parcel_id','year','doy','Date15'))
#
# Sys.setlocale('LC_TIME','en_GB')
#
# out_gridded_plot_all <- ggplot(loc_data,aes(y=infections*25000, x=Date15)) +
#   geom_line(colour='blue') +
#   geom_line(aes(y=stagecode*25000/4), colour='red',size=.6) +
#   geom_line(aes(y=ground_biomass), colour='darkgreen',size=.75) +
#   geom_line(aes(y=org_biomass), colour='darkgreen',linetype=2,size=.75) +
#   geom_hline(yintercept=c(0,0.25,.5,.75,1)*25000,linetype=3,colour='red') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   xlab('Date') + ylab('') + ggtitle('Biotic risk estimation (blue line), stagecode (red line),\nbiomass (green line) and panicle biomass (dashed green line)') +
#   facet_grid(parcel_num~year) +
#   theme_bw()
#
# out_gridded_plot_biomass <- ggplot(loc_data[parcel_num==100001,],aes(y=ground_biomass/1E3, x=Date15)) +
#   geom_line(colour='darkgreen') +
#   geom_line(aes(y=org_biomass/1E3), colour='darkgreen',linetype=2) +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biomass (T/ha)') + ggtitle('Greek local simulation, parcel K1') +
#   annotate('text',x=as.POSIXct('2015-05-01'),y=max(loc_data[parcel_num==100001,ground_biomass/1E3]),label='dotted: panicle',hjust='left') +
#   theme_bw()# +
# #   theme(legend.position="none",
# #         axis.title.x=element_blank(),
# #         axis.text.x=element_blank())
# out_gridded_plot_stagecode <- ggplot(loc_data[parcel_num==100001,],aes(y=stagecode, x=Date15)) +
#   geom_line(colour='red') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Stagecode') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()# +
# #   theme(legend.position="none",
# #         axis.title.x=element_blank(),
# #         axis.text.x=element_blank())
# out_gridded_plot_risk <- ggplot(loc_data[parcel_num==100001,],aes(y=pot_infections, x=Date15)) +
#   geom_line(colour='blue') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biotic risk') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()# +
#   # theme(legend.position="none")
# out_gridded_plot_risk_reg <- ggplot(raw_data[int_id==3011,],aes(y=pot_infections, x=Date15)) +
#   geom_line(colour='blue') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biotic risk') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()# +
#   # theme(legend.position="none")
#
# library(gridExtra)
# g_biomass <- ggplotGrob(out_gridded_plot_biomass)
# g_stagecode <- ggplotGrob(out_gridded_plot_stagecode)
# g_risk <- ggplotGrob(out_gridded_plot_risk)
# g_risk_reg <- ggplotGrob(out_gridded_plot_risk_reg)
# maxWidth = grid::unit.pmax(g_biomass$widths[2:5], g_stagecode$widths[2:5], g_risk$widths[2:5], g_risk_reg$widths[2:5])
# g_biomass$widths[2:5] <- as.list(maxWidth)
# g_stagecode$widths[2:5] <- as.list(maxWidth)
# g_risk$widths[2:5] <- as.list(maxWidth)
# g_risk_reg$widths[2:5] <- as.list(maxWidth)
#
# p4 <- arrangeGrob(g_biomass, g_stagecode, g_risk, nrow = 3, heights = c(1, .95, 1.05))
# p5 <- arrangeGrob(g_biomass, g_stagecode, g_risk_reg, nrow = 3, heights = c(1, .95, 1.05))
#
#
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr.png'),3000,2500,res=300)
# print(out_gridded_plot_all)
# dev.off()
#
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1.png'),3000,2000,res=300)
# plot(p5)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_biomass.png'),3000,900,res=300)
# plot(g_biomass)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_stagecode.png'),3000,900,res=300)
# plot(g_stagecode)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_risk.png'),3000,900,res=300)
# plot(g_risk_reg)
# dev.off()
#
#
#
# out_gridded_plot_biomass_14 <- ggplot(loc_data[parcel_num==100001&year==2014,],aes(y=ground_biomass/1E3, x=Date15)) +
#   geom_line(colour='darkgreen') +
#   geom_line(aes(y=org_biomass/1E3), colour='darkgreen',linetype=2) +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biomass (T/ha)') + ggtitle('Greek local simulation, parcel K1') +
#   annotate('text',x=as.POSIXct('2015-05-01'),y=max(loc_data[parcel_num==100001,ground_biomass/1E3]),label='dotted: panicle',hjust='left') +
#   theme_bw()
# out_gridded_plot_biomass_15 <- ggplot(loc_data[parcel_num==100001&year==2015,],aes(y=ground_biomass/1E3, x=Date15)) +
#   geom_line(colour='darkgreen') +
#   geom_line(aes(y=org_biomass/1E3), colour='darkgreen',linetype=2) +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biomass (T/ha)') + ggtitle('Greek local simulation, parcel K1') +
#   annotate('text',x=as.POSIXct('2015-05-01'),y=max(loc_data[parcel_num==100001,ground_biomass/1E3]),label='dotted: panicle',hjust='left') +
#   theme_bw()
# out_gridded_plot_stagecode_14 <- ggplot(loc_data[parcel_num==100001&year==2014,],aes(y=stagecode, x=Date15)) +
#   geom_line(colour='red') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Stagecode') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()
# out_gridded_plot_stagecode_15 <- ggplot(loc_data[parcel_num==100001&year==2015,],aes(y=stagecode, x=Date15)) +
#   geom_line(colour='red') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Stagecode') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()
# raw_data$pot_infections_2 <- ifelse(raw_data$doy<182|raw_data$doy>243,0,raw_data$pot_infections)
# out_gridded_plot_risk_reg_14 <- ggplot(raw_data[int_id==3011&year==2014,],aes(y=pot_infections_2, x=Date15)) +
#   geom_line(colour='blue') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biotic risk') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()
# out_gridded_plot_risk_reg_15 <- ggplot(raw_data[int_id==3011&year==2015,],aes(y=pot_infections_2, x=Date15)) +
#   geom_line(colour='blue') +
#   xlim(as.POSIXct('2015-05-01'),as.POSIXct('2015-10-31')) +
#   facet_grid(.~year) +
#   xlab('Date') + ylab('Biotic risk') + ggtitle('Greek local simulation, parcel K1') +
#   theme_bw()
#
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_biomass_2014.png'),1500,900,res=300)
# plot(out_gridded_plot_biomass_14)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_biomass_2015.png'),1500,900,res=300)
# plot(out_gridded_plot_biomass_15)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_stagecode_2014.png'),1500,900,res=300)
# plot(out_gridded_plot_stagecode_14)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_stagecode_2015.png'),1500,900,res=300)
# plot(out_gridded_plot_stagecode_15)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_risk_2014.png'),1500,900,res=300)
# plot(out_gridded_plot_risk_reg_14)
# dev.off()
# png(file.path(nrworking_dir,'luigi/misc/Minor_analyses/160318_plot_outputs_loc_gr/outputs_loc_gr_K1_risk_2015.png'),1500,900,res=300)
# plot(out_gridded_plot_risk_reg_15)
# dev.off()
