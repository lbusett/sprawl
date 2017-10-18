library(sprawl)
library(sf)
library(raster)
library(rasterVis)
library(ggspatial)

# multispectral operations
in_file <- system.file("extdata/OLI_test",
                       "oli_multi_1000.tif", package = "sprawl.data")
in_rast <- read_rast(in_file)
# retrieve info
info <- get_rastinfo(in_rast, stats = T)
info
info$stats

info <- get_raststats(in_rast, quantiles = T)
info
b1 <- subset(info$quants, band == 1)
plot(b1$val, b1$quant)

info <- get_raststats(in_rast, quantiles = T, hist = T)

histplot <- plot_rasthist(in_rast, type = "line")
histplot + theme_bw()
histplot + theme_bw() + xlim(-200, 20000)


#Multi band plot with selection of bands
plot_rast_gg(in_rast, basemap = "stamenbw", bands_to_plot = c(1,4),
             palette_name = "RdYlGn",
             na.value = 0, na.color = "transparent",
             title = "OLI", subtitle = "Band 2 vs band 4",
             facet_row = 1)

#Adjusting limits and changing palette
in_rast <- read_rast(system.file("extdata/REYE_test",
                                     "REYE_2016_185_gNDVI.tif",
                                     package = "sprawl.data"))
names(in_rast) = "REYE_June"
#vanilla, with borders added
borders <- get_boundaries("ITA", 3)
plot_rast_gg(in_rast,
             palette_name = "RdYlBu",
             title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
             borders_layer = borders, borders_txt_field = "NAME_3")

# limits adjusted on the 10th and 99.9th percentile to remove very low
# values outliers and better use the palette on high values (note that
# outliers can be set as transparent!) + increase maxpixels to plot at
# full resolution.
plot_rast_gg(in_rast, maxpixels = 50E4,
             palette_name = "RdYlBu",
             zlims = c(0.10, 0.999), zlims_type = "percs",
             outliers_style = "censor",
             outliers_colors = c("black", "purple"),
             borders_layer = borders, borders_txt_field = "NAME_3",
             borders_txt_color = "darkgreen",
             scalebar_dist = 1.5,
             title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
             {})

in_shapefile <- "D:/tmp_sprawl/shape/IT_Field_data_static_2016_20170216.shp"

# read the file and select useful columns
in_vect <- read_vect(in_shapefile) %>%
  dplyr::select(parcel_id, parcel_num, crop_type, sowing_doy, yield) %>%
  dplyr::filter(!is.na(crop_type)) %>%
  dplyr::mutate(crop_type = factor(crop_type))
head(in_vect)

plot(in_vect)
plot(in_vect[1])
plot_vect(in_vect)

bounds <- get_boundaries("ITA", 3) %>%
  dplyr::filter(NAME_1 == "Lombardia")

plot_vect(in_vect,
          fill_var = "crop_type",
          palette_name = "hue",
          borders_layer = bounds,
          borders_txt_field = "NAME_3",
          scalebar_dist = 1)

# Mask and buffer the raster
mask_rast <- mask_rast(in_rast,
                  in_vect,
                  buffer = -15)

plot_rast_gg(mask_rast,
             palette_name = "RdYlGn",
             borders_layer = in_vect,
             na.color = "transparent")

# Extract statistics on polygons
stats <- extract_rast(in_rast,
                      in_vect,
                      id_field = "parcel_num")

stats
stats$stats
#
# diss_vect <- dissolve_vect(in_vect, dissolve_var = "crop_type")
# plot_vect(diss_vect, fill_var = "crop_type")
# stats <- extract_rast(in_rast,
#                       diss_vect,
#                       id_field = "crop_type")

# MOnica ----

in_rast <- read_rast("D:/tmp_sprawl/monica/Villa_ref_5bands_centauro.tif.0.tif")
in_vect <- read_vect("D:/tmp_sprawl/monica/Centauro_plots.shp")[1:16,]

stats <- extract_rast(in_rast, in_vect, id_field = "ID")
head(stats)

plot <- ggplot(stats$stats) +
  geom_point(aes(x = as.factor(ID), y = avg, color = as.factor(ID))) +
  geom_errorbar(aes(x = as.factor(ID), ymax = avg + sd, ymin = avg -sd)) +
  facet_wrap(~band_name, scales = "free_y") + theme_bw()

allpixels    <- stats$alldata
allpixels$ID <- as.factor(allpixels$ID)
plot <- ggplot(allpixels) +
  geom_boxplot(aes(x = ID, y = value, fill = ID), position = position_dodge(width = 1)) +
  facet_wrap(~band_name, scales = "free_y")


in_rast <- read_rast("D:/tmp_sprawl/monica/Villa_VI_centauro.tif.0.tif")[[1]]
stats_VI <- extract_rast(in_rast, in_vect, id_field = "ID", full_data = TRUE,
                         join_geom = F)

allpixelazs_VI <- stats_VI$alldata
allpixels_VI$ID <- as.factor(allpixels_VI$ID)

plot <- ggplot(allpixels_VI) +
  geom_boxplot(aes(x = ID, y = value, fill = ID),
               position = position_dodge(width = 1)) +
  theme_bw()+ facet_wrap(~band_name, scales = "free_y")

#save

write.csv(allpixels, file = "D:/tmp_sprawl/test_all.csv")
write.csv(allpixels_VI, file = "D:/tmp_sprawl/test_all_VI.csv")

write.csv(stats_VI$stats, file = "D:/tmp_sprawl/test_stats_VI.csv")
write.csv(allpixels_VI, file = "D:/tmp_sprawl/test_all_VI.csv")




# on time series ----

in_polys <- read_vect(system.file("extdata/shapes","lc_polys.shp",
                                  package = "sprawl.data"))
in_rast  <- read_rast(system.file("extdata/MODIS_test", "EVIts_test.tif",
                              package = "sprawl.data"))
in_rast <- setZ(in_rast, doytodate(8*(1:46), 2013))
names(in_rast) <- getZ(in_rast)

in_rast
in_polys


in_polys
plot_vect(in_polys, fill_var = "category",
          borders_layer = get_boundaries("PHL", 1),
          borders_txt_field = "NAME_1")

#Let's have a look at some bands:
plot_rast_gg(in_rast, bands_to_plot = c(1,10,20,30),
             zlims = c(0,7000),
             borders_layer = in_polys,
             palette_name = "RdYlGn",
             maxpixels = 5E4)

extracted_values <- extract_rast(in_rast,
                                 in_polys,
                                 id_field = "lc_type")
head(extracted_values$stats)

stats <- extracted_values$stats
sf::st_geometry(stats) = NULL
write.csv(stats, file = "D:/tmp_sprawl/test.csv")

# compute standard error
stats$se <- stats$sd / stats$n_pix_val^0.5

plot <- plot_stats(stats, group_var = "lc_type")

plot_stats <- function(indata,
                       group_var,
                       err_column = "se") {
  plot <- ggplot(indata) + theme_bw()
  plot <- plot + geom_line(aes_string(x = "date", y = "avg", color = group_var))
  plot <- plot + geom_errorbar(aes(x = date, ymin = avg - se, ymax = avg + se))
  plot <- plot + facet_wrap(eval(group_var))
  plot <- plot + scale_x_date(date_labels = "%d %b")

}

# dissolve the polys on category

in_polys_cat <- dissolve_vect(in_polys,
                              dissolve_var = "category")
in_polys
plot_vect(in_polys, fill_var = "category",
          borders_layer = get_boundaries("PHL", 1),
          borders_txt_field = "NAME_1")
extracted_values <- extract_rast(in_rast,
                                 in_polys_cat,
                                 id_field = "category")
stats <- extracted_values$stats
stats$se <- stats$sd / stats$n_pix_val^0.5
plot <- plot_stats(stats, group_var = "category")
