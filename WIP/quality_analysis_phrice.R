library(raster)
library(rgdal)
library(data.table)
library(gdalUtils)
library(sprawl)
library(stringr)
library(magrittr)
library(xts)
library(ggplot2)

in_mod       <- "/home/lb/Temp/quality_phenorice/IND/QUALITY_ts_input_2013_137_2015_009.dat"
inmodrast    <- stack(in_mod)
mod_proj     <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
in_ext       <- extent(inmodrast)
in_fishnet   <- "/home/lb/Temp/quality_phenorice/IND/mask_fishnet.tif"
in_maskfile  <- "/home/lb/Temp/quality_phenorice/IND/mask_quality.tif"


NAvalue(inmodrast) <- 255

years <- str_split(names(inmodrast), "_", simplify  = TRUE)[,3]
temp  <- str_split(names(inmodrast), "_", simplify  = TRUE)[,4]
doys  <- str_split(temp,".dat", simplify  = TRUE)[,1]

dates <- doytodate(doys = as.numeric(doys), year = as.numeric(years))

inmodrast <- setZ(inmodrast, dates, name = 'time')
inmodcropped <- crop (inmodrast, extent(raster(in_fishnet)))
data_quality <- fastzonal(inmodcropped, in_fishnet, verbose = T, out_format = 'dframe')

in_mask <- raster(in_maskfile)
rice_cells <- which(getValues(in_mask) == 1)

sel_cells <- which(as.numeric(names(data_quality)[2:length(names(data_quality))]) %in% rice_cells)
sel_data  <- data_quality[,c(1,sel_cells)]

seldata_melt <- melt(sel_data, id.vars = "date")



# p <- ggplot(seldata_melt, aes(x = date, y = value, group = date)) +
#   geom_boxplot() +
#   theme_bw()
# p
#
# seldata_avg <- data.frame(date = sel_data[,1], value = rowSums(sel_data[(2:dim(sel_data)[2])])/(dim(sel_data)[2]-1))
#
# p <- ggplot(seldata_avg, aes ( x = date, y = value))
# p <- p + geom_point() + theme_bw()
# p



sel_data_phl  = sel_data
seldata_melt_phl  = seldata_melt
seldata_avg_phl = seldata_avg
sel_data_phl$country = "PHL"
seldata_melt_phl$country = "PHL"
seldata_avg_phl$country = "PHL"

sel_data_ita  = sel_data
seldata_melt_ita  = seldata_melt
seldata_avg_ita = seldata_avg
sel_data_ita$country = "ITA"
seldata_melt_ita$country = "ITA"
seldata_avg_ita$country = "ITA"

sel_data_ind  = sel_data
seldata_melt_ind  = seldata_melt
seldata_avg_ind = seldata_avg
sel_data_ind$country = "IND"
seldata_melt_ind$country = "IND"
seldata_avg_ind$country = "IND"



sel_datatot     = rbind(sel_data_phl, sel_data_ind, sel_data_ita)
sel_datamelttot = rbind(seldata_melt_phl, seldata_melt_ind, seldata_melt_ita)
sel_dataavgttot = rbind(seldata_avg_phl, seldata_avg_ind, seldata_avg_ita)

p <- ggplot(sel_datamelttot, aes(x = date, y = value, group = date))
p <- p + geom_boxplot() + facet_wrap(~country)
p

percs$value = as.factor(percs$value)
p <- ggplot(percs, aes(x = date, y = freq, group = value))
p <- p + geom_line(aes(colour = value)) + theme_bw() + facet_wrap(~country)
p <- p + scale_x_date(date_breaks = "3 month") +
  scale_colour_manual(values = c("green","blue","red")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p



percs = sel_datamelttot %>%
 group_by(date, country, value) %>%
 summarise (n = n()) %>%
 mutate(freq = 100*n / sum(n))


data_ita = subset(percs, country =="ITA" & value == "2")

p2 = p1 + geom_line(data = subset(percs, country =="ITA" & value == "2"), aes(x = date + 365, y = freq/100, group = value),color = "black", lty = 'dashed')
#p2 = p2 + geom_line(data = subset(percs, country =="ITA" & value == "1"), aes(x = date + 365, y = freq/100, group = value),color = "blue", lty = 'dashed')
#:p2 = p2 + geom_line(data = subset(percs, country =="ITA" & value == "0"), aes(x = date + 365, y = freq/100, group = value),color = "green", lty = 'dashed')
p2 = p2
data_phl = subset(percs, country =="PHL" & value == "2")
p8 = p7 + geom_line(data = subset(percs, country =="PHL" & value == "2"),
                    aes(x = date +365, y = freq/100, group = value),color = "black", lty = 'dashed')

#p2 = p2 + geom_line(data = subset(percs, country =="ITA" & value == "1"), aes(x = date + 365, y = freq/100, group = value),color = "blue", lty = 'dashed')
#:p2 = p2 + geom_line(data = subset(percs, country =="ITA" & value == "0"), aes(x = date + 365, y = freq/100, group = value),color = "green", lty = 'dashed')
p8

p9 = p6 + geom_line(data = subset(percs, country =="IND" & value == "2"),
                    aes(x = date, y = freq/100, group = value),color = "black", lty = 'dashed')

p9
  scale_colour_manual(values = c("green","blue","red"))

