
in_folder <- "/home/lb/phenorice/bhogendra/input/VI_16Days_250m/DOY/"
out_folder <- "/home/lb/Source/git/phenoricev3/test_data/bhogendra/Original_MODIS/VI_16Days_250m/DOY/"
in_clip_shape <- readshape("/home/lb/phenorice/bhogendra/Clipper_PHL_sinu.shp")
in_nodata = out_nodata = -1
out_format = "ENVI"

lb_batch_clip(in_folder, out_folder, in_clip_shape, in_pattern = "*.dat$",
              in_nodata, out_nodata, out_format, recursive = TRUE)

