in_folder = "/home/lb/Source/phenorice/data/IT_Full/Ancillary/"
out_folder = "/home/lb/Source/phenorice/data/IT_Clipped/Ancillary/"
in_clip_shape = read_shape("/home/lb/Source/phenorice/data/IT_Full/Ancillary/Clipper/Clipper_IT/Clip_area_it.shp")

in_nodata = out_nodata = 32767
out_format = "ENVI"

lb_batch_clip(in_folder, out_folder, in_clip_shape, in_pattern = "*.dat$", in_nodata, out_nodata, out_format, recursive = TRUE)

