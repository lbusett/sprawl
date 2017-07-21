library(raster)

infile  <- "/home/lb/Temp/pheno_it_2017/IT/Outputs4/2017/Phenorice_out_2017_033_2017_345.dat"
in_rast = raster::brick(infile)
levelplot(infile, maxpixels = 1000000000)

tmp <- raster("/home/lb/Temp/pheno_it_2017/VI_16Days_250m_v6/EVI/MOD13Q1_EVI_2003_001.dat")

sowdates     <- in_rast[[2]]
proj4string(sowdates) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
sowdates_cut <- cuttails_rast(sowdates)
extent(sowdates_cut) <- extent(tmp)


aggr_rast    <- raster::raster("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R3_LAI/2017/MOD/IT_LAI_MOD_2017_001.tif")
sowdates_agg <- aggregate_rast(sowdates_cut, aggr_rast)


reclass_matrix <- tibble::tribble(~start, ~end, ~new,
                               0,    96,   NA, # values >=0 and < 1 will be set to NA
                               96,  169,   1,  # values >=1 and < 5 will be set to 1
                               169, 1000, NA)   # values >=11 and < 100 will be set to NA
mask = reclass_rast(raster(insss), reclass_matrix = reclass_matrix, to_file = T)

sowdates_final <- sowdates_agg * mask

outfile <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R2_Phenology/2017/MinDoys/IT_Phenology_MinDoys_2017_001.tif"
writeRaster(sowdates_final, filename = outfile, NA.flag = -999, overwrite = T)


file <- raster("/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EP_R2_Phenology/2016/MinDoys/IT_Phenology_MinDoys_2016_002.tif")
