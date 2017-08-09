#' @title get_PGIS
#' @description FUNCTION_DESCRIPTION
#' @param schema_name PARAM_DESCRIPTION
#' @param table_name PARAM_DESCRIPTION
#' @param con PARAM_DESCRIPTION
#' @param geom_field PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @author Lorenzo Busetto, phD - Derived from: http://www.r-bloggers.com/load-postgis-geometries-in-r-without-rgdal
#' @export 
#' @importFrom DBI dbGetQuery
#' @importFrom rgdal make_EPSG
#' @importFrom rgeos readWKT
#' @importFrom sp SpatialPolygonsDataFrame
#' @examples 
#'  

get_PGIS <- function(schema_name, table_name, con, geom_field = NULL, verbose = F) {
  
  
  if (length(geom_field) == 0) {
    geom_field <- "geom"
  }
  # Querying DB
  cat("Querying DB ", "\n")
  col_names <- DBI::dbGetQuery(con, paste0("SELECT column_name FROM information_schema.columns WHERE table_schema = '", 
                                           schema_name, "' AND table_name = '", table_name, "'"))
  proj      <- DBI::dbGetQuery(con, paste0("SELECT Find_SRID ('", schema_name, "', '", table_name, "', '", 
                                      geom_field, "')"))
  
  nogeom_col <- which(col_names != geom_field)
  var_string <- paste0(paste(col_names$column_name[nogeom_col], collapse = ", "), 
                       ", ST_AsText(", geom_field, 
                       ") AS wkt_geometry")
  
  strSQL <- paste0("SELECT ", var_string, " FROM ", paste(schema_name, table_name, sep = "."))
  dfTemp <- dbGetQuery(con, strSQL)
  row.names(dfTemp) <- dfTemp$gid
  
  EPSG <- rgdal::make_EPSG()
  p4s  <- EPSG[which(EPSG$code == proj$find_srid), "prj4"]
  message("Retrieving data and converting to SP object")
  for (i in seq(nrow(dfTemp))) {
    if (verbose) 
      message("Retrieving feature ", i, " of ", nrow(dfTemp))
    
    if (i == 1) {
      spTemp <- rgeos::readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    } else {
      spTemp <- rbind(spTemp, rgeos::readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s))
    }
  }
  out <- sp::SpatialPolygonsDataFrame(spTemp, dfTemp[, 1:(length(col_names$column_name) - 1)])
  return(out)
}
