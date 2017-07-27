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
#   get correspondence between names, var, seas, and year. Create a data frame ####
#   with an associated column that will tell us in which order to pick the
#   bands form the original stack

names <- tibble::tibble(name = names(in_rast_sos))
split_names <- stringr::str_split_fixed(names$name, "_", 3)
names <- cbind(names, split_names, or_order = seq(1:length(names$name))) %>%
  `colnames<-`(c("name", "var", "season", "year", "order")) %>%
 dplyr::arrange(Year, Season)

r <- raster::stack(in_rast_sos)

for (band in 1:raster::nlayers(in_rast_sos)) {

  pippo[[band]] <- in_rast_sos[[names$order[band]]]

}

#   ____________________________________________________________________________
#   order in which we have to pick the bands from the stack is in "names$order"   ####


#   ____________________________________________________________________________
#   Crop on an area of interest                                             ####
#

country    <- "PHL"
boundaries <- get_boundaries(country, level = 0)
boundaries <- st_as_sf(boundaries)

dir.create("/home/lb/my_data/prasia/PHL")

t1 <- Sys.time()
sos <- mask_rast(in_rast_sos, boundaries, crop = TRUE, to_file = TRUE,
                  out_rast = "/home/lb/my_data/prasia/PHL/SOS_PHL.tif")
names(sos) <-
Sys.time() - t1
#   ____________________________________________________________________________
#   Decircularize dates rasters                                             ####
#

