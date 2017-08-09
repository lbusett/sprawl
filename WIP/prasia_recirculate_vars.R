library(sprawl)
library(sf)
library(dplyr)
library(tidyr)

mosaics_folder <- "/home/lb/my_data/prasia/mosaics/by_year"

in_sos_files <- list.files(mosaics_folder, pattern = "sos*", full.names = T)

names <- tibble::tibble(fullname = in_sos_files,
                        shortname = basename(tools::file_path_sans_ext(in_sos_files)))
split_names <- stringr::str_split_fixed(names$shortname, "_", 3)

names <- cbind(names, split_names, or_order = seq(1:length(names$fullname))) %>%
  `colnames<-`(c("fullname", "basename", "var", "season", "year", "order")) %>%
 dplyr::arrange(year, season)

ordered_files <- names$fullname

temp_vrt <- tempfile(fileext = ".vrt")
pp = gdalUtils::gdalbuildvrt(ordered_files, temp_vrt, separate = T)


 gdal_translate -ot Int16 -of Gtiff -co "COMPRESS=DEFLATE" /tmp/RtmpyrONRv/file7360614d92a6.vrt /tmp/RtmpyrONRv/prova.tif


test_out <- file.path("/home/lb/my_data/prasia/mosaics/tests/test1.tif")
dir.create(dirname(test_out))
test <- raster::writeRaster(in_rast_nseas[[1:5]], test_out, options = c("COMPRESS=DEFLATE"))

test_vrt <- tempfile(fileext = ".vrt")
gdalUtils::gdalbuildvrt(in_rast_nseas[[1:5]], test_vrt)


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

