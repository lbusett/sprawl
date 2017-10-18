library(sprawl)
library(sf)
library(raster)
library(rasterVis)

# raster i/0 and categrorization

in_file   <- "D:/tmp_sprawl/rast/LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"

in_rast <- read_rast(in_file)
plot(in_rast, maxpixels = 5E4)
levelplot(in_rast, maxpixels = 5E4)

# crop the map on a extent of interest

ita_bound <- get_boundaries("ITA", level = 0 )
ita_map   <- crop_rast(in_rast, ita_bound)
plot(ita_bound[1])
plot(ita_map, maxpixels = 5E4)

ita_map   <- crop_rast(in_rast,
                       ext_object = get_boundaries("ITA", level = 0),
                       out_file = "D:/tmp_sprawl/rast/LC/ITA_LC.tif")
plot(ita_map, maxpixels = 5E4)
ita_map

# Reclassify

class_matrix <- tibble::tribble(
~start, ~end, ~new, ~ label,
-Inf,   10,   NA, NA,
  10,   11,   1,  "Cropland",
  11,   20,   2,  "Grassland",
  20,   40,   1,  "Cropland",
  40,   50,   3,  "Mixed Crop/NatVeg",
  50,   70,   4,  "Broadleaved Forest",
  70,   81,   5,  "Needleleaved Forest",
  81,   100,  6,  "Mixed Forest",
  100,  190,  7,  "Shrubland/Sparse Veg",
  190,  200,  8,  "Urban Areas",
  200,  210,  9,  "Bare Areas",
  210,  220,  10,  "Water",
  220,  Inf,  11, "Snow/Ice"
)

in_rast_rec <- categorize_rast(ita_map,
                               class_matrix = class_matrix,
                               out_file = "D:/tmp_sprawl/rast/LC/LC_ESA_reclass.tif",
                               overwrite = TRUE)
levels(in_rast_rec)

plot(in_rast_rec)
levelplot(in_rast_rec)

plot_rast_gg(in_rast_rec)

plot_rast_gg(in_rast_rec,
             borders_layer = get_boundaries("ITA", level = 2),
             palette_name = "manual",
             leg_colors = c("yellow", "khaki","sienna", "forestgreen",
                            "darkgreen", "sandybrown", "firebrick4",
                            "burlywood2", "grey50", "blue", "gray95" ),
             maxpixels = 500E4
             )


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
