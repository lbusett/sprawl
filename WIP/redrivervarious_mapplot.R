library(tidyverse)
library(sp)
library(lubridate)
library(xts)
library(raster)
library(rgdal)
library(RColorBrewer)

infile  <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Output/2016/Phenorice_out_2015_297_2017_033.dat"
inshape <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Ancillary/VNM_adm_shp/VNM_adm2.shp"

insos_1 <- raster(infile, band = 2)
insos_1 <- stack(insos_1, insos_1)
inshp   <- readshape(inshape)
data_stat <- fastzonal_new(insos_1,inshp, out_format = "dframe", id_field = "NAME_2", verbose = T, small = F, summary_data = TRUE, comp_quant = T)

  
  for (feat_id in unique(feat_names)){
    ts$zones_new <- as.character(NA)
    feat_code <- zone_cropped$mdxtnq[which(feat_id == zone_cropped@data[, eval(id_field)])]
    which_replace <- which(ts$zones == feat_id)
    ts$zones_new[which_replace] = feat_id
  }
  
  
  as_tibble() %>% 
  gather("NAME_2", "SOS_1", -date)

inshp@data <- left_join(inshp@data, data_stat)

out = raster::extract(insos_1,inshp, fun=mean, na.rm = T, sp = T)
out2_S1 = subset(out, !is.na(out$Phenorice_out_2015_297_2017_033))


mapview(out2, zcol = "Phenorice_out_2015_297_2017_033", legend = T, alpha.regions = 0.6, na.color = "transparent", 
        color = colorRampPalette(brewer.pal(9, "RdYlGn")))


infile  <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Output/2016/Phenorice_out_2015_297_2017_033.dat"
inshape <- "/media/nas-s4a/nr_working/shared/PhenoRice/Processing/RedRiver/Ancillary/VNM_adm_shp/VNM_adm2.shp"
insos_2 <- raster(infile, band = 3)
out2 = raster::extract(insos_2,inshp, fun=mean, na.rm = T, sp = T)
out2_s2 = subset(out2, !is.na(out$Phenorice_out_2015_297_2017_033))
mapview(out2_s2, zcol = "Phenorice_out_2015_297_2017_033", legend = T, alpha.regions = 0.6, na.color = "transparent", 
        color = colorRampPalette(brewer.pal(9, "RdYlGn")))


pal <- "RdYlGn"

pal <- colorNumeric(brewer.pal(9, "RdYlGn"), seq(160,210, 10),
                    na.color = "transparent")
 leaflet() %>% addTiles() %>%
       addRasterImage(insos_2, colors = pal, opacity = 0.8) %>%
       addLegend(pal = pal,title = "Start of Season", values = seq(160,210, 10))


 r = insos_1
  pal <- colorNumeric(brewer.pal(9, "RdYlGn"), seq(30,80, 10),
                     na.color = "transparent")
  leaflet() %>% addTiles() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>%
   addRasterImage(insos_1, colors = pal, opacity = 0.95, group = "r") %>%
   addLegend(pal = pal, title = "SOS - Spring Season", values = seq(30,80, 10))%>% 
    addLayersControl(overlayGroups = "r",
                     options = layersControlOptions(collapsed = FALSE))
 
 
 

 
 
 r = insos_2
 
  leaflet() %>% addTiles() %>% 
   addProviderTiles(providers$Esri.WorldImagery) %>%
   addRasterImage(r, colors = pal, opacity = 0.95, group = "r") %>%
   addLegend(pal = pal, values = seq(160,210, 10),
             title = "SOS - Winter Season") %>% 
    addLayersControl(overlayGroups = "r",
                     options = layersControlOptions(collapsed = FALSE))
       
