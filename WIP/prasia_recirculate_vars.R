library(sprawl)
library(sf)

in_folder <- "/home/lb/my_data/prasia/mosaics/"



#   ____________________________________________________________________________
#   create full raster stacks and cahnge band names                         ####

in_rast_nseas  <- raster::stack(list.files(in_folder, pattern = glob2rx("nseas*"), full.names = T))
in_rast_sos    <- raster::stack(list.files(in_folder, pattern = glob2rx("sos*"), full.names = T))
in_rast_eos    <- raster::stack(list.files(in_folder, pattern = glob2rx("eos*"), full.names = T))
in_rast_pos    <- raster::stack(list.files(in_folder, pattern = glob2rx("pos*"), full.names = T))
in_rast_veglgt <- raster::stack(list.files(in_folder, pattern = glob2rx("veg*"), full.names = T))
in_rast_totlgt <- raster::stack(list.files(in_folder, pattern = glob2rx("tot*"), full.names = T))
in_rast_cumevi <- raster::stack(list.files(in_folder, pattern = glob2rx("cum*"), full.names = T))


years = seq(2003,2016)
for (y_ind in seq_along(years)) {
  yy = years[y_ind]
  names(in_rast_sos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_sos))
  names(in_rast_eos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_eos))
  names(in_rast_pos)    <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_pos))
  names(in_rast_veglgt) <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_veglgt))
  names(in_rast_totlgt) <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_totlgt))
  names(in_rast_cumevi) <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_cumevi))
  names(in_rast_nseas)  <- gsub(paste0("_all.",y_ind, "$"), paste0("_", yy), names(in_rast_nseas ))
}

#   ____________________________________________________________________________
#   Crop on an area of interest                                             ####
#

country    <- "PHL"
boundaries <- get_boundaries(country, level = 0)
boundaries <- st_as_sf(boundaries)

dir.create("/home/lb/my_data/prasia/PHL")

t1 <- Sys.time()
test <- mask_rast(in_rast_sos, boundaries, crop = TRUE, to_file = TRUE,
                  out_rast = "/home/lb/my_data/prasia/PHL/sos_phl.tif")
Sys.time() - t1
#   ____________________________________________________________________________
#   Decircularize dates rasters                                             ####
#

