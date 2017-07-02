library(stringr)
library(mapview)
library(tidyverse)
library(sf)
library(foreach)

# area = "Alto_Adige"
# area = "VDA"
area       <-  "VDA"
out_folder <- file.path("/home/lb/nr_working/lorenzo/Temp_Mico/results", area)
dir.create(out_folder, showWarnings = FALSE)

in_data      <- paste0("/home/lb/nr_working/lorenzo/Temp_Mico/", area, "/VI_16Days_250m_v6/Time_Series/RData/MOD13Q1_MYD13Q1_NDVI_49_2000_17_2017_RData.RData")
in_ts        <- get(load(in_data))
in_data_ui   <- paste0("/home/lb/nr_working/lorenzo/Temp_Mico/", area, "/VI_16Days_250m_v6/Time_Series/RData/MOD13Q1_MYD13Q1_QA_usef_49_2000_17_2017_RData.RData")
in_ts_ui     <- get(load(in_data_ui))
in_data_rely <- paste0("/home/lb/nr_working/lorenzo/Temp_Mico/", area, "/VI_16Days_250m_v6/Time_Series/RData/MOD13Q1_MYD13Q1_Rely_49_2000_17_2017_RData.RData")
in_ts_rely   <- get(load(in_data_rely))
in_data_qa   <- paste0("/home/lb/nr_working/lorenzo/Temp_Mico/", area, "/VI_16Days_250m_v6/Time_Series/RData/MOD13Q1_MYD13Q1_QA_qual_49_2000_17_2017_RData.RData")
in_ts_qa     <- get(load(in_data_rely))

old_path <- "/home/lbusetto/nas-s4a/nr_working/"
new_path <- "/home/lb/nr_working/"

for (layer in 1:dim(in_ts)[3]){
  in_ts@layers[[layer]]@file@name <- str_replace(in_ts@layers[[layer]]@file@name, old_path, new_path)
  in_ts_ui@layers[[layer]]@file@name <- str_replace(in_ts_ui@layers[[layer]]@file@name, old_path, new_path)
  in_ts_rely@layers[[layer]]@file@name <- str_replace(in_ts_rely@layers[[layer]]@file@name, old_path, new_path)
  in_ts_qa@layers[[layer]]@file@name <- str_replace(in_ts_qa@layers[[layer]]@file@name, old_path, new_path)
}

shp_frane        <- readshape("/home/lb/nr_working/lorenzo/Temp_Mico/shapes/Frane_da_provare_Buffer5000m.shp", stringsAsFactors = TRUE)
shp_frane_points <- readshape("/home/lb/nr_working/lorenzo/Temp_Mico/shapes/Frane_da_provare.shp", stringsAsFactors = TRUE) %>% 
  sf::st_transform(proj4string(in_ts)) %>% 
  as("Spatial")

frane_buffered <- pBuffer(shp_frane_points, c(2000,2000)) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(1,2) %>% 
  dplyr::mutate(OID_ = seq(1:16))

sf::st_crs(frane_buffered) <- proj4string(in_ts)


# Extract data from time series

start.time          <- Sys.time()
in_time_series      <- fastzonal(in_ts, frane_buffered, summ_data = FALSE, id_field = "Name")$ts_full
in_time_series_ui   <- fastzonal(in_ts_ui, frane_buffered, summ_data = FALSE, id_field = "Name")$ts_full
in_time_series_rely <- fastzonal(in_ts_rely, frane_buffered, summ_data = FALSE, id_field = "Name")$ts_full
in_time_series_qa   <- fastzonal(in_ts_qa, frane_buffered, summ_data = FALSE, id_field = "Name")$ts_full
end.time            <- Sys.time()
time.taken          <- end.time - start.time
time.taken

save(in_time_series,in_time_series_ui, in_time_series_rely,in_time_series_qa, file = file.path(out_folder, paste0(area, ".RData")))

# Function to transform fastzonal "pixel output" back to raster

fz_to_stack <- function(indata, verbose = T){
  out_xyz   <- list()
  
  indata    <- indata %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2])
 
  for (bb in 1:max(indata$band_n)){
    # if(verbose) message(bb)
    in_xyz <- indata %>% 
      dplyr::filter(band_n == bb) %>%  
      tibble::as_tibble() %>% 
      tibble::as_data_frame() %>% 
      dplyr::select(x,y,value) %>% 
      sf::st_set_geometry(NULL)
    out_xyz[[bb]] <- raster::rasterFromXYZ(in_xyz)
  }
  return(stack(out_xyz))
}

out_vi_ts                <- fz_to_stack(in_time_series) 
out_ui_ts                <- fz_to_stack(in_time_series_ui)
out_rely_ts              <- fz_to_stack(in_time_series_rely)
out_qa_ts                <- fz_to_stack(in_time_series_qa)

proj4string(out_vi_ts)   <- proj4string(in_ts)
proj4string(out_ui_ts)   <- proj4string(in_ts)
proj4string(out_rely_ts) <- proj4string(in_ts)
proj4string(out_qa_ts) <- proj4string(in_ts)
dir.create(out_folder)
out_vi_ts_f   <- file.path(out_folder, "ndvi_raw.tif")
out_ui_ts_f   <- file.path(out_folder, "ui.tif")
out_rely_ts_f <- file.path(out_folder, "rely.tif")
out_qa_ts_f   <- file.path(out_folder, "qa.tif")
writeRaster(out_vi_ts, filename = out_vi_ts_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT2S")
writeRaster(out_ui_ts, filename = out_ui_ts_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT1U")
writeRaster(out_rely_ts, filename = out_rely_ts_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT1U")
writeRaster(out_rely_ts, filename = out_qa_ts_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT1U")


where_bad    <- which(in_time_series_ui$value > 5 | in_time_series_rely$value > 1)
where_zeroqa <- which(in_time_series_qa$value == 0)
time_series_NA <- in_time_series
time_series_NA$value[where_bad] <- NA
time_series_NA$value[where_zeroqa] <- in_time_series$value[where_zeroqa]

out_vi_ts_NA <- fz_to_stack(time_series_NA)
proj4string(out_vi_ts_NA) = proj4string(in_ts)
out_vi_ts_NA_f   <- file.path(out_folder, "ndvi_filtered.tif")
writeRaster(out_vi_ts_NA, filename = out_vi_ts_NA_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT2S")

# ui_data = in_time_series_ui %>% 
#   dplyr::filter(Name == "Gandellino", N_PIX == 1)
# 
# rely_data = in_time_series_rely %>% 
#   dplyr::filter(Name == "Gandellino", N_PIX == 1)

filt_func = function(vi){
  
  w = array(data = 1, dim = 725)
  na = which(is.na(vi$value))
  w[vi$Rely == 0] = 1
  w[vi$Rely >= 5] = 0.05
  w[vi$Rely> 0 & vi$Rely < 5] = 0.1
  # vi$value[na] = 0
  # w[na] = 0
   # browser()
  vi$value[is.na(vi$value)] = 0
  out <- whit2(vi$value, 5, w)
  return(data.frame(band_n = vi$band_n, date = vi$date, raw = vi$value, smoothed = out, OID_ = vi$OID_))
}

in_time_series$Rely = in_time_series_rely$value

out_smooth <- in_time_series %>% 
  group_by(Name, N_PIX) %>% 
  do(filt_func(.)) %>% 
  left_join(in_time_series) %>% 
  st_as_sf() %>% 
  arrange(Name ,band_n, date, N_PIX )
  
names(out_smooth)[c(6, 8)] = c("value", "pippo")

out_vi_ts_smooth <- fz_to_stack(out_smooth)
proj4string(out_vi_ts_smooth) = proj4string(in_ts)
out_vi_ts_smooth_f   <- file.path(out_folder, "ndvi_smoothed.tif")
writeRaster(out_vi_ts_smooth, filename = out_vi_ts_smooth_f, overwrite = TRUE, bandorder = "BIL", datatype = "INT2S")




out = out_smooth %>% filter(Name == "profonda 1")


pl = ggplot(out) +theme_bw()
pl = pl + geom_line(aes(x = date, y = raw), color = "black")
pl = pl + geom_line(aes(x = date, y = value), color = "red")
pl + facet_wrap(~N_PIX)

pl = ggplot(out) + theme_bw()
pl = pl + geom_boxplot(aes(x = date, y = value, group = date), color = "black", outlier.shape = NA)
pl


p <- plotly_build(pl)

p$data <- lapply(p$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})

p 
# for(pix in 1:)
# 
# 
# pix_data = in_time_series %>% 
#   dplyr::filter(Name == "Gandellino", N_PIX == 1)
# 
# y = pix_data$value
# plot(whit1(y, 1e1))
# 
#      
#           (pix_data$value, lambda = 1600, d = 2))
# 
# p = ggplot() + theme_bw()
# p = p + geom_point(data = pix_data, aes(x = date, y = value)) +
#   geom_line(data = pix_data, aes(x = date, y = value))
# p = p + geom_point(data = a, aes(x = date, y = value)) +
#   geom_line(data = a, aes(x = date, y = value), color = "red")
# 
# ts = (as.timeSeries(pix_data$value))
# setTime(ts) <- pix_data$date
# ts = (smoothLowess(ts, f = 0.2)) %>% 
#   as.data.frame() %>% 
#   mutate(date = row.names(.)) %>% 
#   as_tibble() %>% 
#   gather(var, value, -date)
# ts
# 
# p <- ggplot(ts, aes(x = date, y = value, color = var))+
#   geom_point()+
#   geom_line()
# 
# 
# p  
# 
# 
# data <- as.matrix(pix_data)
# charvec <- rownames(pix_data)
# Close <- timeSeries(data, charvec, units = "Close")
# 
# p
# 
# smoothed_pix <- sgolayfilt(pix_data$value, p = 3, n = 9)
# 
# a = data.frame(date = pix_data$date, y = as.numeric(z))
# 
# mapview(shp_frane)
# mapview(shp_frane_points)
