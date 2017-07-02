library(tidyverse)
library(sf)
filein    <- "/home/lbusetto/nas-s4a/nr_data/vector_layers/Italy/Lombardia/Particelle_agricole_dati/2015/Particelle/Particelle_agricole_Lodi/shape_lodi.shp"
indata    <- openshape(filein, stringsAsFactors = FALSE)
indataval <- st_is_valid(indata)
indataval <- indata[indataval == TRUE, ]
dissdata  <- st_union(indataval, by_feature = FALSE)
dissdata2  <- st_combine(indataval, dTolerance = 30)
fileout   <-  "/home/lbusetto/temp/buttami/particelle_LO_dissfull2.shp"
st_write(dissdata, fileout)
