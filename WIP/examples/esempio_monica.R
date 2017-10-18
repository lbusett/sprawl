# MOnica

in_rast <- read_rast("D:/tmp_sprawl/monica/Villa_ref_5bands_centauro.tif.0.tif")
in_vect <- read_vect("D:/tmp_sprawl/monica/Centauro_plots.shp")[1:16,]

stats <- extract_rast(in_rast, in_vect, id_field = "ID")
head(stats)

plot <- ggplot(stats$stats) +
  geom_bar(aes(x = factor(ID), y = avg, fill = factor(ID)), stat = "identity") +
  facet_wrap(~band_name, scales = "free_y")

allpixels <- stats$alldata
allpixels$ID <- as.factor(allpixels$ID)
plot <- ggplot(allpixels) +
  geom_boxplot(aes(x = ID, y = value, fill = ID), position = position_dodge(width = 1)) +
  facet_wrap(~band_name, scales = "free_y")


in_rast <- read_rast("D:/tmp_sprawl/monica/Villa_VI_centauro.tif.0.tif")[[1]]
stats_VI <- extract_rast(in_rast, in_vect, id_field = "ID", full_data = TRUE,
                         join_geom = F)
allpixels <- stats_VI$alldata
allpixels$ID <- as.factor(allpixels$ID)
plot <- ggplot(allpixels) +
  geom_boxplot(aes(x = ID, y = value, fill = ID),
               position = position_dodge(width = 1)) +
  theme_bw()
