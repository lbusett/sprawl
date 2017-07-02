library (ggplot2)
library(plyr)
library(gridExtra)
library(scales)
library(sprawl)

setwd()


file_in = '/home/lb/nr_working/shared/PhenoRice/Documentes/Paper/mirco/data/Phenorice_dataset_graphs_new.csv'
file_in_vi = '/home/lb/nr_working/shared/PhenoRice/Documentes/Paper/mirco/data/Phenorice_dataset_vis.csv'

data_in = read.csv(file_in, header = T)
data_in_vi = read.csv(file_in_vi, header = T)
names(data_in)

levels(data_in$Country) = c('IND','ITA','PHL')
levels(data_in_vi$Country) = c('IND','ITA','PHL')

data_sub = droplevels(subset(data_in, Variable %in% c("Min_DOY_1st_Quarter","Min_DOY_2nd_Quarter", "Min_DOY_3rd_Quarter","Min_DOY_4th_Quarter",
                                           "Max_DOY_1st_Quarter" ,"Max_DOY_2nd_Quarter","Max_DOY_3rd_Quarter","Max_DOY_4th_Quarter")))

data_sub = data_sub[!((data_sub$Site == 1) & (data_sub$Country == "IND")),]
data_sub$Site [(data_sub$Site == 4) & (data_sub$Country == "IND")] = 1
data_sub$type = NA
# data_ind_vi = droplevels(subset(data_in_vi, Country == 'PHL' ))
data_in_vi$date = lb_doytodate(data_in_vi$Doy, 2014)
data_in_vi = data_in_vi[!((data_in_vi$Site == 1) & (data_in_vi$Country == "IND")),]
data_in_vi$Site [(data_in_vi$Site == 4) & (data_in_vi$Country == "IND")] = 1

phrice_plot = function(data, cy = cy, site = site, title = NULL, ann = ann) {
  data_vi = droplevels(subset(data, Country == cy & Site == site))
  data_ph = droplevels(subset(data_sub, Country == cy & Site == site ))

  data_ph$type [1:4] =  "Crop Establishment"
  data_ph$type [5:8] =  "Crop Flowering"
  data_ph$type = as.factor(data_ph$type)
  data_ph = rbind(data_ph, data_ph)
  data_ph$y = -0.1
  data_ph$date = lb_doytodate(data_ph$Value, 2014)
  # if (cy == "IND") {
  # 	data_ph$date = data_ph$datde
  # }
  data_ph$y [((length(data_ph$date)/2)+1):length(data_ph$date)] <- 1

  data_ph$quart = rep(c(1,2,3,4,5,6,7,8),2)
  data_ph = droplevels(subset(data_ph, Country == cy & Site == site & Value != 0))
  if (cy == 'IT') {data_ph$date = data_ph$date-8}
  if (cy == 'IND') {data_ph$date = data_ph$date-8}
  # data_ph$date[data_ph$date < as.Date('2014-08-01')] = data_ph$date[data_ph$date < as.Date('2014-08-01')]-8


  p = ggplot(data_vi) + lb_theme_bw()
  p = p + ggtitle(title)

  p = p + geom_line(aes(x = date, y = EVI_Smooth/10000, colour = 'EVI - Smoothed'),size = 1)
  p = p + geom_line(aes(x = date, y = NDFI/10000, colour = 'NDFI'),size = 1)
  p = p + geom_point(aes(x = date , y = EVI_Raw/10000, colour = 'EVI - Raw') ,pch = 5)
  p = p + geom_line(data = data_ph, aes(x = date,  y = y, color = type, group = quart), lty = 3,size = 1.1)
  p = p + guides(colour=guide_legend(override.aes=list(linetype=c(3,3,0,1,1),shape=c(NA,NA,5,NA,NA))))
  if ((cy == 'IT') |  (cy == 'IND')){
  	p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2013-09-01"), as.Date("2014-10-31")),date_labels = "%b" )
  } else {
  	p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2013-10-31"), as.Date("2014-12-31")),date_labels = "%b" )
  }
  p = p + coord_cartesian(ylim = c(0,0.8))
  p = p + ggtitle(title) + xlab('Date')+ ylab("VI") + scale_colour_manual('',values = c('orange','chartreuse3','black','red','blue'))

  if ((cy == 'IT') |  (cy == 'IND')){
  	p = p + annotate("text", x = as.Date("2013-10-30"), y = 0.80, label = paste0("Lat: ", as.character(round(data_vi$N[1], 2)), " - Lon: ", as.character(round(data_vi$E[1], 2))), size = 3)
  } else {
  	p = p + annotate("text", x = as.Date("2013-12-30"), y = 0.80, label = paste0("Lat: ", as.character(round(data_vi$N[1], 2)), " - Lon: ", as.character(round(data_vi$E[1], 2))), size = 3)
  }

  p = p + theme(legend.position= 'bottom',legend.direction = 'horizontal', legend.background = element_rect(colour = 'black'))
  p = p + theme(legend.text= element_text())
  # browser()
  # p = p + annotate("text", aes(x = 0,y = 1) ,data = ann, label = ann$txt)
  # if (!(cy == 'PHL' & site == '3')) {p = p + guides(colour=FALSE)}
   # p = p + guides(colour=FALSE)+ theme(axis.title = element_text(size = 17),
   # 																		axis.text.x = element_text(size = 12),
   # 																		axis.text.y = element_text(size = 12),
   # 																		plot.title = element_text(size = 15),
   # 																		legend.text = element_text(size = 12),
   # 																		legend.title = element_text(size = 12))
  p
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
p1 = phrice_plot(data_in_vi, cy = 'ITA', site = "1", title = "ITA: Rice - Fallow", ann = data.frame(txt = 'a)'))
# legend <- get_legend(p)
p2 = phrice_plot(data_in_vi, cy = 'IND', site = "5", title = "IND: Rice - Other Crop", ann = data.frame(txt ='b)'))
p3 = phrice_plot(data_in_vi, cy = 'PHL', site = "5", title = "PHL: Other Crop - Rice", ann = data.frame(txt ='c)'))

p4 = phrice_plot(data_in_vi, cy = 'IND', site = "1", title = "IND: Rice - Fallow", ann = data.frame(txt ='d)'))
p5 = phrice_plot(data_in_vi, cy = 'PHL', site = "1", title = "PHL: Rice - Fallow", ann = data.frame(txt ='e)'))

p6 = phrice_plot(data_in_vi, cy = 'IND', site = "2", title = "IND: Rice - Pulse - Rice", ann = data.frame(txt ='f)'))
p7 = phrice_plot(data_in_vi, cy = 'PHL', site = "2", title = "PHL: Rice - Other - Rice", ann = data.frame(txt ='g)'))

p8 = phrice_plot(data_in_vi, cy = 'IND', site = "3", title = "IND: Rice - Rice - Rice", ann = data.frame(txt ='h)'))
p9 = phrice_plot(data_in_vi, cy = 'PHL', site = "3", title = "PHL: Rice - Rice - Rice", ann = data.frame(txt ='i)'))


blank = ggplot()+geom_blank()+ theme(panel.background = element_rect(fill = 'white'))
p_tot = grid.arrange(p1,p4,p5,blank,p2,p3,blank, p6,p7,blank, p8,p9)


p = ggplot(data_in_vi, aes(x = date, y = EVI_Raw )) + theme_bw()
p = p + geom_point() + geom_line(aes(x = date, y = EVI_Smooth)) + facet_grid(Country~Site, scales = "free_x")
p = p + scale_x_date(date_breaks = "2 month", limits = c(as.Date("2013-05-01"), as.Date("2014-06-01")),date_labels = "%m %d")
p1

