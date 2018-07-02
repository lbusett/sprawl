library(sf)
library(purrr)
library(dplyr)

nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(3035)

distance.func <- function(polygon){
  max(st_distance(st_cast(polygon, "POINT"), st_centroid(polygon)))
}
dist <- list()
for (i in seq_along(nc[[1]])) dist[[i]] <- distance.func(nc[i,]) %>%
  unlist()
head(unlist(dist))

a <- nc[1,]
b <- a %>% st_set_crs(NA)
c <- sf::st_centroid(a)
d <- sf::st_centroid(b)

all.equal(c, d)

dist <- list()
dist  <- list(); for (i in seq_along(nc[[1]])) dist[[i]] <- distance.func(nc[i,])


library(purrr)

distance.func_lapply <- function(polygon, crs){
   # browser()
  polygon <- st_sfc(polygon, crs = crs)
   # browser()
  centroid <-  st_centroid(polygon)
  max(st_distance(st_cast(polygon, "POINT"),centroid))
  # browser()
}

dist_lapply <- lapply(st_geometry(nc), distance.func_lapply, crs = st_crs(nc))
dist_map <- purrr::map(st_geometry(nc), distance.func_lapply, crs = crsin)

all.equal(dist, dist_lapply)
all.equal(dist, dist_map)
crsin <-  st_crs(nc)
a <- microbenchmark::microbenchmark(
  forloop = {for (i in seq_along(nc[[1]])) dist[[i]] <- distance.func(nc[i,])},
  map     = {dist_map <- purrr::map(st_geometry(nc), distance.func_lapply, crs = crsin)},
  lapply  = {dist_lapply <- lapply(st_geometry(nc),  distance.func_lapply, crs = crsin)}, times = 10)


