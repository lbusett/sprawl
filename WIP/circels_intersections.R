library(tibble)
library(dplyr)
library(sf)

ncircles <- 15
rmax     <- 120
x_limits <- c(-70,70)
y_limits <- c(-30,30)
set.seed(100) 
xy <- data.frame(
  id = paste0("id_", 1:ncircles), 
  val = paste0("val_", 1:ncircles),
  x = runif(ncircles, min(x_limits), max(x_limits)),
  y = runif(ncircles, min(y_limits), max(y_limits)),
  stringsAsFactors = FALSE) %>% 
  as_tibble()

polys <- st_as_sf(xy, coords = c(3,4)) %>% 
  st_buffer(runif(ncircles, min = 1, max = 20)) 
plot(polys[1])

library(stringr)
get_difference_region <- function(cur, x, input_polys, keep_columns=c("id")){
  # browser()
   x <- x[!x==cur] # remove self 
  len <- length(x)
  input_poly_sfc <- st_geometry(input_polys)
  input_poly_attr <- as.data.frame(as.data.frame(input_polys)[, keep_columns])
  
  # base poly
  res_poly <- input_poly_sfc[[cur]]
  res_attr <- input_poly_attr[cur, ]
  
  # substract the intersection parts from base poly
  if(len > 0){
    for(i in 1:len){
      res_poly <- st_difference(res_poly, input_poly_sfc[[x[i]]]) 
    }
  }
  if (length(res_poly) > 0 ) {
    # print(paste0(i, "   ", res_poly))
    # res_attr$geom = res_poly
    # 
    # cbind(res_attr, data.frame(geom=st_as_text(res_poly)), stringsAsFactors = FALSE)
    no_int_feature =  st_sf(res_attr,  stringsAsFactors = FALSE, st_sfc(res_poly))
    
    return(no_int_feature)
  } else {
    
      return()
    }

}


get_intersection_region <- function(cur, x, input_polys, keep_columns=c("id"), sep="&"){
  x <- x[!x<=cur] # remove self and remove duplicated obj 
  len <- length(x)
  input_poly_sfc <- st_geometry(input_polys)
  input_poly_attr <- as.data.frame(as.data.frame(input_polys)[, keep_columns])
  
  res_df <- data.frame()
  if(len > 0){
    for(i in 1:len){
      res_poly <- st_intersection(input_poly_sfc[[cur]], input_poly_sfc[[x[i]]])
      res_attr <- list()
      for(j in 1:length(keep_columns)){
        pred_attr <- str_split(input_poly_attr[cur, j], sep, simplify = TRUE)
        next_attr <- str_split(input_poly_attr[x[i], j], sep, simplify = TRUE)
        res_attr[[j]] <- paste(sort(unique(c(pred_attr, next_attr))), collapse=sep)
      }
      res_attr <- as.data.frame(res_attr)
      colnames(res_attr) <- keep_columns
      single_part <- sf
      res_df <- rbind(res_df, cbind(res_attr, data.frame(geom=st_as_text(res_poly))))
    }
  }
  return(res_df)
}


# init
close_df <- data.frame()
open_sf <- polys

# main loop
while(!is.null(open_sf)) {
  flag <- st_intersects(open_sf, open_sf)
  close_df = list()
  for(i in 1:length(flag)) {
    cur_df <- get_difference_region(i, flag[[i]], open_sf, keep_column = c("id", "val"))
    close_df[[i]] <- cur_df
  }
  close_df <- data.table::rbindlist(close_df)
  cur_open <- data.frame()
  for(i in 1:length(flag)) {
    cur_df <- get_intersection_region(i, flag[[i]], open_sf, keep_column = c("id", "val"))
    cur_open <- rbind(cur_open, cur_df)
  }
  if(nrow(cur_open) != 0) {
    cur_open <- cur_open[row.names(cur_open %>% select(-geom) %>% distinct()),]
    open_sf <- st_as_sf(cur_open, wkt="geom")
  }
  else{
    open_sf <- NULL
  }
}

close_sf <- close_df %>% 
  mutate(geom = as.character(geom)) %>% 
  st_as_sf(close_df, wkt="geom")
close_sf
plot(close_sf)
