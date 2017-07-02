files  = list.files (
  "/home/lb/nr_working/shared/PhenoRice/Processing/RSE_Paper/PHL/Inputs/Time_Series/VI_16Days_250m/EVI/",
  pattern = "*.dat$", full.names = TRUE)

ord = as.numeric(paste0(str_split(names(stack), "_", simplify = TRUE)[,3], str_split(names(stack), "_", simplify = TRUE)[,4]))

files = files [order(ord)[28:73]]
stack = stack(files)
crop_stack = crop(stack, readshape("data/clip_shape.shp"))
crop_stack = setZ(crop_stack, dates)
writeRaster(crop_stack, filename = "data/EVI_ts.tif", format = "GTiff", options = ("compress=DEFLATE"), overwrite = T)


in_rast = get(load("data/EVI_ts.RData"))
