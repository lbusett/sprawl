# Setup ----

library(lwgeom) # devtools::install_github('r-spatial/lwgeom)
library(tidyverse)
library(sf)
library(esri2sf) # devtools::install_github('yonghah/esri2sf')
library(rbenchmark)
library(knitr)

# Create the new sf function: st_intersects_any ----

st_intersects_any <- function(x, y) {
  st_intersects(x, y) %>%
    map_lgl(~ length(.x) > 0)
}

# Load data ----
# NC counties

nc <- read_sf(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(32119)

nc_1e4 <- list(nc) %>%
  rep(times = 1e2) %>%
  reduce(rbind)

# NC watersheds

url <- "https://services.nconemap.gov/secure/rest/services/NC1Map_Watersheds/MapServer/2"

nc_wtr <- esri2sf(url)
## Warning: package 'httr' was built under R version 3.4.2
##
## Attaching package: 'jsonlite'
## The following object is masked from 'package:purrr':
##
##     flatten
## [1] "Feature Layer"
## [1] "esriGeometryPolygon"

nc_wtr <- st_transform(nc_wtr, 32119) %>%
  st_simplify(dTolerance = 100) # simplify the waterbodies geometries

# plot the data

par(mar = rep(.1, 4))
plot(st_geometry(nc), lwd = 1)
plot(st_geometry(nc_wtr), col = alpha("blue", .3), lwd = 1.5, add = TRUE)

# Benchmark the two approaches

cols <- c("elapsed", "relative")

bm_sf_small <- benchmark({
  st_intersects_any(nc, nc_wtr)
}, columns = cols, replications = 1)

bm_sf_dplyr_small <- benchmark({
  nc %>% mutate(INT = map_lgl(geometry, st_intersects_any, y = nc_wtr))
}, columns = cols, replications = 1)
## Warning: package 'bindrcpp' was built under R version 3.4.2

bm_sf_large <- benchmark({
  st_intersects_any(nc_1e4, nc_wtr)
}, columns = cols, replications = 1)

bm_sf_dplyr_large <- benchmark({
  int_old <- nc_1e4 %>% transmute(INT = map_lgl(geometry, st_intersects_any, y = nc_wtr))
}, columns = cols, replications = 1)

bm_sf_dplyr_large_fast <- benchmark({
  int_new <- nc_1e4 %>% mutate(INT = st_intersects_any(., nc_wtr))
}, columns = cols, replications = 1)
bm_sf_dplyr_large_fast

tests <- list(bm_sf_small, bm_sf_dplyr_small, bm_sf_large, bm_sf_dplyr_large)

tbl <- tibble(
  TEST = c("bm_sf_small", "bm_sf_dplyr_small", "bm_sf_large", "bm_sf_dplyr_large"),
  ELAPSED = map_dbl(tests, "elapsed")
)

kable(tbl,format = "markdown", padding = 2)
