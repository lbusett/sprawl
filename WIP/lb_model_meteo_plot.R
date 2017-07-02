library(data.table)
library(ggplot2)
library(gdalUtils)
library(RPostgreSQL)
# library(sprawl)
library(dplyr)
library(RColorBrewer)


# example:

# warmplot('es',1:10, 'potinf') --> Plots potential infections for parcels 1 to 10 in spain
# warmplot('es',1:10, 'stage') --> Plots stagecode for parcels 1 to 10 in spain

# nrworking_dir <- 'W:'
nrworking_dir <- '/home/lb/nr_working/'
# ermes_dir <- 'V:'
ermes_dir <- '/home/lb/projects/ermes/'

cc = 'it'

# open the DB
source(file.path(nrworking_dir,'luigi/code/Ermes/WARM_DB/opencon.R'))

warmplot = function(cc = 'it',parcels = c(1:4), plotvar = 'agb'){


  # get the data
  loc_data_full = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass,
                                                 org_biomass, cold_ster, pot_infections FROM loc_", cc,".out_warm ", "WHERE year = 2015")))
  loc_data_sow =data.table(dbGetQuery(con,paste0("SELECT parcel_id, sowing_doy FROM loc_",cc,".location_management ","WHERE year = 2015")))

  loc_data = filter(loc_data_full, parcel_id)
  loc_data = filter(loc_data_full, parcel_id  %in% unique(loc_data_full$parcel_id)[parcels])
  sdates = filter(loc_data_sow, parcel_id %in% unique(loc_data_full$parcel_id)[parcels])
  loc_data = left_join(loc_data, sdates)
  loc_data$date = lb_doytodate(loc_data$doy, 2015)
  loc_data$parcel_id = as.factor(loc_data$parcel_id)
  loc_datam = melt(loc_data, id.vars = c('parcel_id', 'date','year', 'doy'))

  plotlist = list()
  cnt = 0
  if ("yield" %in% plotvar) {

    cnt = cnt +1
    plotdata = droplevels(subset(loc_datam, variable == 'org_biomass'))
    plotdata$value = plotdata$value/1000
    pyield = ggplot(plotdata, aes(x = date, y = value, group = parcel_id)) + lb_theme_bw(x_ang = 45)
    pyield = pyield + geom_line()
    pyield = pyield + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
    pyield = pyield + theme(axis.text = element_text(size = 11,
                                                     hjust = 1, vjust = 0.25)) + labs(x = "Date",
                                                                                      y = "Yield [ton/ha]")
    pyield = pyield + facet_wrap(~parcel_id)
    plotlist[[cnt]] = pyield
  }

  if ("agb" %in% plotvar) {
    cnt = cnt +1
    plotdata = droplevels(subset(loc_datam, variable == 'ground_biomass'))
    pagb = ggplot(plotdata, aes(x = date, y = value/1000, group = 1)) + lb_theme_bw(x_ang = 45)
    pagb = pagb + geom_line()
    pagb = pagb + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
    pagb = pagb + theme(axis.text = element_text(size = 11,
                                                 hjust = 1, vjust = 0.25)) + labs(x = "Date",
                                                                                  y = "Above Ground Biomass [ton/ha]")
    pagb = pagb + facet_wrap(~parcel_id)
    plotlist[[cnt]] = pagb
  }

  if ("stage" %in% plotvar) {
    cnt = cnt +1
    plotdata = droplevels(subset(loc_datam, variable == 'stagecode'))
    plotdata = mutate(plotdata, stage = cut(value, breaks = c(-0.5,0,1,1.3,1.7,2,2.5,3,5), include.lowest = F, right = T,
                                            labels = c('Pre-Sowing','Emergence','Post-Emergence','Tillering','Panicle Initiaition', 'Flowering',
                                                       'Grain Filling', 'Maturation')))
    anndf = data.frame(x = rep(as.Date("2015-09-15"), 6*length(parcels)),
                       y = rep(c(1,1.3,1.7,2,2.5,3),6* length(parcels)),
                       text = rep(c('Emersión','Ahijamiento','Iniciación de la panícula',
                                    'Floración','Desarrolo del grano de arroz', 'Madurez' ), length(parcels)),
                       parcel_id = rep(unique(plotdata$parcel_id), each = 6))

    pstage = ggplot(plotdata, aes(x = date, y = value, group = parcel_id)) + lb_theme_bw(x_ang = 45)
    # pstage = pstage  + geom_line(aes(color = 'stage'), size = 1.2)
    pstage = pstage  + geom_line(size = 1.2)
    pstage = pstage + geom_text(data = anndf, aes(x = x, y = y, label = text, group = NULL))
    pstage = pstage + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-30')))
    pstage = pstage + theme(axis.text = element_text(size = 11,hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Development Stage")
    # pstage = pstage + scale_colour_manual('Phase',values = brewer.pal(9,"Set2"))
    pstage = pstage + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
                  legend.position = c(0.11, 0.67))

    pstage = pstage + geom_hline(yintercept = rep(c(1,1.3,1.7,2,2.5,3,4),each = length(parcels)), lty = 2) + guides(color = F)
    pstage = pstage + facet_wrap(~parcel_id)
    plotlist[[cnt]] = pstage
  }

  if ("inf" %in% plotvar) {
    cnt = cnt +1
    plotdata = droplevels(subset(loc_datam, variable == 'infections'))
    plotdata = plotdata %>%
      mutate(value=replace(value, date <= as.Date('2015-06-01'), 0)) %>%
      as.data.frame()
    plotdata= plotdata %>%
      mutate(value=replace(value, date >= as.Date('2015-10-31'), 0)) %>%
      as.data.frame()
    plotdata = mutate(plotdata, Risk = cut(value, breaks = c(0,0.33,0.66,1),
                                           include.lowest = T, labels = c('Low','Medium','High')))
    pinf = ggplot(plotdata, aes(x = date, y = value*100, group = parcel_id)) + lb_theme_bw(x_ang = 45)
    pinf = pinf + geom_line(color = 'black') + geom_point(aes(color = Risk))
    pinf = pinf + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
    pinf = pinf + theme(axis.text = element_text(size = 11,
                                           hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Potential Infection Risk [%]")
    cols = c('green','orange','red')
    pinf = pinf + scale_colour_manual(values = cols, drop = FALSE)
    pinf = pinf + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
              legend.position = c(0.07, 0.84))
    pinf = pinf + facet_wrap(~parcel_id)
    plotlist[[cnt]] = pinf
  }

  if ("potinf" %in% plotvar) {
    cnt = cnt +1
    plotdata = droplevels(subset(loc_datam, variable == 'pot_infections'))
    plotdata = plotdata %>%
      mutate(value=replace(value, date <= as.Date('2015-06-01'), 0)) %>%
      as.data.frame()
    plotdata= plotdata %>%
      mutate(value=replace(value, date >= as.Date('2015-08-30'), 0)) %>%
      as.data.frame()
    plotdata = mutate(plotdata, Risk = cut(value, breaks = c(0,0.33,0.66,1),
                                           include.lowest = T, labels = c('Low','Medium','High')))
    pinf = ggplot(plotdata, aes(x = date, y = value*100, group = parcel_id)) + lb_theme_bw(x_ang = 45)
    pinf = pinf + geom_line(color = 'black') + geom_point(aes(color = Risk))
    pinf = pinf + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
    pinf = pinf + theme(axis.text = element_text(size = 11,
                                                 hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Potential Infection Risk [%]")
    cols = c('green','orange','red')
    pinf = pinf + scale_colour_manual(values = cols, drop = FALSE)
    pinf = pinf + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
                        legend.position = c(0.07, 0.84))
    pinf = pinf + facet_wrap(~parcel_id)
    plotlist[[cnt]] = pinf
  }
  plotlist

}

# Biomass graphs ----
# cc = 'es'
#
# loc_data = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass, org_biomass, cold_ster, pot_infections FROM loc_", cc,".out_warm ",
#                                             "WHERE parcel_id = 'ES52346237A04400023A' AND year = 2015")))
# loc_data_full = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass,
#                                                  org_biomass, cold_ster FROM loc_", cc,".out_warm ", "WHERE year = 2015")))
# loc_data_sow =data.table(dbGetQuery(con,paste0("SELECT parcel_id, sowing_doy FROM loc_",cc,".location_management ","WHERE year = 2015")))
#
# parc = 40
# loc_data = filter(loc_data_full, parcel_id == unique(loc_data_full$parcel_id)[parc])
# sdate = filter(loc_data_sow, parcel_id == unique(loc_data_full$parcel_id)[parc])[1]
# sdate
#
# loc_data$date = lb_doytodate(loc_data$doy, 2015)
# p = ggplot(loc_data, aes(x = date, y = org_biomass/1000)) + lb_theme_bw(x_ang = 45)
# p = p + geom_line(color = 'black')
# p = p + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
# p = p + theme(axis.text = element_text(size = 11,
#                                        hjust = 1, vjust = 0.25)) + labs(x = "Date",
#                                                                         y = "Above Ground Biomass [ton/ha]")
# p
#
# # infection graphs ----
# #
# cc = 'it'
# loc_data_full = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass, org_biomass, cold_ster FROM loc_", cc,".out_warm ",
#                                                  "WHERE year = 2015")))
# loc_data = filter(loc_data_full, parcel_id == unique(loc_data_full$parcel_id)[2])
#
# loc_data$date = lb_doytodate(loc_data$doy, 2015)
#
# l = loc_data %>%
#   mutate(infections=replace(infections, date <= as.Date('2015-06-01'), 0)) %>%
#   as.data.frame()
# l = l %>%
#   mutate(infections=replace(infections, date >= as.Date('2015-08-30'), 0)) %>%
#   as.data.frame()
# l = mutate(l, Risk = cut(infections, breaks = c(0,0.33,0.66,1), include.lowest = T, labels = c('Low','Medium','High')))
# p = ggplot(l, aes(x = date, y = infections*100,)) + lb_theme_bw(x_ang = 45)
# p = p + geom_line(color = 'black') + geom_point(aes(color = Risk))
# p = p + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-15')))
# p = p + theme(axis.text = element_text(size = 11,
#                                        hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Potential Infection Risk [%]")
# cols = c('green','orange','red')
# p = p + scale_colour_manual(values = cols)
# p + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
#           legend.position = c(0.07, 0.84))
#
#
# # stagecode graphs ----
# cc = 'gr'
# loc_data = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass, org_biomass, cold_ster FROM loc_", cc,".out_warm ",
#                                             "WHERE parcel_id = 'ES52346237A04400023A' AND year = 2015")))
# loc_data_full = data.table(dbGetQuery(con,paste0("SELECT parcel_id, year, doy, infections, stagecode, ground_biomass,
#                                                  org_biomass, cold_ster FROM loc_", cc,".out_warm ", "WHERE year = 2015")))
# loc_data_sow =data.table(dbGetQuery(con,paste0("SELECT parcel_id, sowing_doy, variety FROM loc_",cc,".location_management ","WHERE year = 2015")))
#
# parc = 2
# loc_data = filter(loc_data_full, parcel_id == unique(loc_data_full$parcel_id)[parc])
# sdate = filter(loc_data_sow, parcel_id == unique(loc_data_full$parcel_id)[parc])
# sdate
# loc_data$date = lb_doytodate(loc_data$doy, 2015)
# loc_data = mutate(loc_data, stage = cut(stagecode, breaks = c(-0.5,0,1,1.3,1.7,2,2.5,3,5), include.lowest = F, right = T,
#                                         labels = c('Pre-Sowing','Emergence','Post-Emergence','Tillering','Panicle Initiaition', 'Flowering',
#                                                    'Grain Filling', 'Maturation')))
# p = ggplot(loc_data, aes(x = date, y = stagecode)) + lb_theme_bw(x_ang = 45)
# p = p  + geom_line(aes(color = stage), size = 1.2)
# p = p + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-30')))
# p = p + theme(axis.text = element_text(size = 11,hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Development Stage")
# p = p + scale_colour_manual('Phase',values = brewer.pal(9,"Set2"))
# p = p + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
#               legend.position = c(0.11, 0.67))
# pfull = p
#
# p = ggplot(loc_data, aes(x = date, y = stagecode)) + lb_theme_bw(x_ang = 45)
# p = p  + geom_line(size = 1.2)
# p = p + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-04-01'), as.Date('2015-09-30')))
# p = p + theme(axis.text = element_text(size = 11,hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Development Stage")
# p = p + theme(legend.background = element_rect(size = 0.5, colour = 'black'),
#               legend.position = c(0.11, 0.67))
# p2 = p
# p2 = p2 + geom_hline(yintercept = c(1,1.3,1.7,2,2.5,3,4), lty = 2)
# p2 = p2 + annotate("text", x = as.Date("2015-09-15"), y = c(0.9,1.2,1.55,1.9,2.3,2.8,3.6),
#                    label = c('Emergence','Post-Emergence','Tillering','Panicle Initiaiton', 'Flowering','Grain Filling', 'Maturation' ))
# p2
#
#
# p3 = pfull
# p3 = p3 + geom_hline(yintercept = c(1,1.3,1.7,2,2.5,3,4), lty = 2) + guides(color = F)
# p3 = p3 + annotate("text", x = as.Date("2015-09-15"), y = c(0.9,1.2,1.55,1.9,2.3,2.8,3.6),
#                    label = c('Emergence','Post-Emergence','Tillering','Panicle Initiaiton', 'Flowering','Grain Filling', 'Maturation' ))
# p3
#
# # Meteo graphs ----
# cc = 'it'
#
# meteo_data = data.table(dbGetQuery(con,paste0("SELECT row_number() OVER ()::integer AS gid,
# parcel_id, year, doy, rainfall, tmax, tmin, radiation, rhmax, rhmin, wind_speed, et0, is_forecast
# FROM loc_it.weather
# LEFT JOIN loc_",cc,".parcels ON parcels.weather_id = weather.weather_id
# WHERE parcel_id IN ('ITC4801813000100023A') AND year = 2016
# ORDER BY parcel_id, year, doy;")))
# meteo_data$date = lb_doytodate(meteo_data$doy, 2015)
# meteo_data = meteo_data  %>% select( tmin,tmax,is_forecast, date) %>% as.data.frame
#
# meteo_datam = melt(meteo_data, id = c('date'))
#
# pmet = ggplot(meteo_data, aes(x = date, y = tmin, color = 'variable')) + lb_theme_bw(x_ang = 45)
# pmet = pmet  + geom_line(color = 'red',size = 1.2) + geom_line (aes(y = tmax), color = 'red', size = 1.2)
# pmet = pmet  + geom_line(aes(color = is_forecast),size = 1.2) + geom_line (aes(y = tmax, color = is_forecast), size = 1.2)
# pmet = pmet + scale_x_date(date_breaks = "2 week", date_labels = "%d-%b", limits = c(as.Date('2015-01-01'), as.Date('2015-09-30')))
# pmet = pmet + theme(axis.text = element_text(size = 11,hjust = 1, vjust = 0.25)) + labs(x = "Date",y = "Minimum / Maximum Temperature [°C]")
# pmet = pmet + theme(legend.background = element_rect(size = 0.5, colour = 'red'),
#                     legend.position = c(0.11, 0.67))
# pmet = pmet + scale_color_manual(values = c('red','blue'))
#
# pmet
#
#
# WHERE
# and
#
# raw_data <- data.table(dbGetQuery(con,paste0('SELECT int_id, year, doy, infections, cum_infections, pot_infections, stagecode FROM reg_',cc,'.temp_out_comp_rh_archive')))
# raw_data =
#   raw_data$Date15 <- as.POSIXct(paste(raw_data$year,raw_data$doy),format='%Y %j')
# raw_data <- raw_data[!is.na(int_id),]
