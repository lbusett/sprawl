#' lb_get_PGIS
#'
#' Derived from: http://www.r-bloggers.com/load-postgis-geometries-in-r-without-rgdal
#' @param layer_name
#' @param con
#'
#' @return
#' @export
#'
#' @examples
lb_get_PGIS = function (schema_name, table_name, con, geom_field = NULL, verbose = F){

  
  if (length(geom_field) == 0) {geom_field = "geom"}
  # Querying DB
  cat("Querying DB ","\n")
  col_names = dbGetQuery(con, paste0("SELECT column_name FROM information_schema.columns WHERE table_schema = '",
                                      schema_name, "' AND table_name = '" , table_name,"'"))
  proj = dbGetQuery(con,paste0("SELECT Find_SRID ('",schema_name,"', '",table_name,"', '",geom_field,"')"))

  nogeom_col =  which(col_names != geom_field)
  var_string =  paste0(paste(col_names$column_name[nogeom_col] , collapse = ', '), ", ST_AsText(",geom_field,") AS wkt_geometry")

  strSQL = paste0("SELECT ",var_string, " FROM ", paste(schema_name,table_name, sep = "."))
  dfTemp = dbGetQuery(con, strSQL)
  row.names(dfTemp) = dfTemp$gid

  # Create spatial polygons
  # To set the PROJ4 string, enter the EPSG SRID and uncomment the
  # following two lines:
  #
  
  EPSG = make_EPSG()
  p4s = EPSG[which(EPSG$code == proj$find_srid), "prj4"]
  feat = NULL
  message("Retrieving data and converting to SP object")
  for (i in seq(nrow(dfTemp))) {
    if (verbose) message("Retrieving feature ",i," of ",nrow(dfTemp))

    if (i == 1) {
      spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
      # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    }
    else {
      spTemp = rbind(spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
        # If the PROJ4 string has been set, use the following instead
        # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
      )
    }
  }
  out = SpatialPolygonsDataFrame(spTemp,dfTemp[,1:(length(col_names$column_name)-1)] )
  return(out)
}
