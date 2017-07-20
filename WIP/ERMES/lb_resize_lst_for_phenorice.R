infold <- "/home/lb/projects/ermes/datasets/rs_products/MODIS/IT/Surf_Temp_8Days_1Km_v6/LST_Day_1km/"
outfold <- "/home/lb/Temp/buttami/templst"
infiles <- list.files(infold, pattern = ".dat$", full.names = T)

ind = 1

infile <- infiles[ind]
outfile <- file.path(outfold, basename(infile))
dir.create(dirname(outfile))

for (infile in seq_along(infiles)) {
  message(infile, "   ", length(infiles))
  outfile <- file.path(outfold, basename(infiles[infile]))
  gdalUtils::gdal_translate(infiles[infile],
                               outfile,
                               # ot = "FLT4S",
                               of = "ENVI",
                               projwin = c(609951.190614, 5090185.1543340003117919,
                                           995195.713968, 4957909.3739160001277924))
}

