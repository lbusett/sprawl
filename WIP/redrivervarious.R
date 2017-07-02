library(mapview)
library(raster)
library(tidyverse)
library(Hmisc)
library(plotly)

in_file <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Input/VI_16Days_250m_v5/Time_Series/RData/MOD13Q1_MYD13Q1_EVI_1_2014_49_2017_RData.RData"
in_stack <- stack(get(load(in_file)))

in_file <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Input/VI_16Days_250m_v5/Time_Series/RData/MOD13Q1_MYD13Q1_NDFI_1_2014_49_2017_RData.RData"
in_stack_ndfi <- stack(get(load(in_file)))

plot(in_stack[[60]])
points <- SpatialPointsDataFrame(click(n=5), data = data.frame("ID" = c(1:5)),proj4string = CRS(proj4string(in_stack))) 
  

data_pts  <- fastzonal(in_stack, points, verbose = T, out_format = "dframe")
data_ndfi <- fastzonal(in_stack_ndfi, points, verbose = T, out_format = "dframe")
  
data_plot  <-  
  data_pts %>%   
  gather(ID,EVI,-date) 

data_plot_NDFI  <-  
  data_ndfi %>%   
  gather(ID,NDFI,-date) 

data_plot <- as_tibble(cbind(data_plot, data_plot_NDFI$NDFI))
  names(data_plot)[4] = "NDFI"

p <- ggplot(data_plot) + 
  geom_smooth(aes(x = date, y = EVI, group = ID), method  = "loess", span = 0.15, color = "red", se = F) + 
  geom_smooth(aes(x = date, y = NDFI, group = ID), method  = "loess", span = 0.15, color = 'blue', se = F) + 
  scale_x_date(date_breaks = "month", date_labels = "%b") + theme_bw()+
  facet_wrap(~ID)+
  ylim(-2000,7000) + xlim(as.Date("2015-11-30"), as.Date("2016-11-30"))

ggplotly(p) 

library(gdalUtils)
in_folder <- "/media/nas-s4a/nr_data/raster_products/Hansen_Mask/treecover/"
a <- raster("/media/nas-s4a/nr_data/raster_products/Hansen_Mask/treecover/Hansen_GFC2015_treecover2000_00N_000E.tif")


ext <- spTransform(SpatialPoints(t(bbox(in_stack)), proj4string = CRS(proj4string(in_stack))), CRS(proj4string(a)))
in_vrt <- gdalbuildvrt(list.files(in_folder, full.names = TRUE),
                       "/media/nas-s4a/nr_data/raster_products/Hansen_Mask/treecover/treecov.vrt", te = extent(ext)[c(1,3,2,4)])





