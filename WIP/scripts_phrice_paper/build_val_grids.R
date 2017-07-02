library(sf)
library(data.table)

cnt <- "PHL"

bound <- get_boundaries(cnt, 0) %>% 
  st_as_sf() %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

md_grd = which(st_intersects(st_as_sf(modis_grid), boundaries) == TRUE)
tiles <- st_as_sf(modis_grid)[md_grd,] 
# %>%
#   st_combine() %>%
#   st_as_sf()
aaa = raster("/home/lb/nr_working/shared/PhenoRice/Processing/RSE_Paper/PHL/Inputs/Time_Series/VI_16Days_250m/EVI/MOD13Q1_EVI_2013_209.dat")
fish <- create_fishnet(tiles, cellsize = 231.656358*9, crop_layer = boundaries, out_shape = "/home/lb/Temp/buttami/prova.shp")
fish <- readshape("/home/lb/Temp/buttami/PHL_2k.shp")
inters <- st_intersects(fish, p)
fishnew <- fish[inters[[1]], ]

writeshape(fishnew, "/home/lb/Temp/buttami/PHL_2k_subset.shp", overwrite = T)

t1 = Sys.time()
b = maskrast(aaa, bound)
time = Sys.time() - t1
bound = as(bound, "Spatial")
t1 = Sys.time()
a = raster::mask(aaa, bound)
time = Sys.time() - t1
