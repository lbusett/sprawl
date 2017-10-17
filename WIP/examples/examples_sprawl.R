library(sprawl)
library(sf)
library(raster)
library(rasterVis)

# raster i/0 and categrorization

in_file   <- "/home/lb/tmp/dati_esempi_sprawl/lc_map/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"

in_rast <- read_rast(in_file)
plot(in_rast, maxpixels = 5E4)
levelplot(in_rast, maxpixels = 5E4)

ita_bound <- get_boundaries("ITA", level = 0 )
plot(ita_bound[1])
ita_map   <- crop_rast(in_rast, ita_bound)
plot(ita_map, maxpixels = 5E4)

reg <- get_boundaries("ITA", level = 1)
plot(reg[1])
lom <- subset(reg, NAME_1 == "Lombardia")
plot(lom[1])

lomb_map <- mask_rast(ita_map, lom, crop = T)
plot(lomb_map)
levelplot(lomb_map)

lomb_map <- set_rastlabels(lomb_map, in_legend)plot(lomb_map)


in_legend <-  read.csv("/media/lb/TOSHIBA EXT/dati_esempi_sprawl/lc_map/Legend_lc_map.csv")
head(in_legend)



# multispectral operations

myrast <- read_rast("/home/lb/tmp/dati_esempi_sprawl/OLI_2016_192_int.tif")

info <- get_rastinfo(myrast)
info

info <- get_rastinfo(myrast, stats = T)
info$stats

info <- get_rastinfo(myrast, stats = T, quantiles = T)
info$stats

info <- get_raststats(myrast, quantiles = T, hist = T)

histplot <- plot_rasthist(myrast, type = "line")
histplot
