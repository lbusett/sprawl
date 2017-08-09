#' @title dbReadSpatial
#' @description Import a spatial table / view / materialised view from a `PostgreSQL`
#' database into a `Spatial*DataFrame` object.
#' @details The function reads a `PostGIS` table / view / materialised view from a `PostgreSQL` database,
#' in which a spatial column (format geom) must be present, and import it in `R` as a *`sp` object
#' `SpatialPixelsDataFrame`, `SpatialLinesDataFrame` or `SpatialPolygonsDataFrame`.
#' The name of the geometric column is automatically retrieved, while the primary key is retrieved
#' only reading a table (in case a view or materialised view is read, it must be specified with the
#' parameter `pkey`).
#'
#' @param conn `DBIConnection` object, as produced by [DBI::dbConnect()].
#' @param name `character` the name of the spatial table / view / materialised view.
#' If the table is not in the 'public' schema, use a two-length character vector in the form
#' \code{c('schema_name','table_name')}.
#' @param pkey `character` (optional) the name of the primary key. This is needed only in the case of
#' views or materialised views (the primary key of a table is automatically retrieved).
#' @param ... other options to be passed to....s
#'
#' @return An object of class `SpatialPixelsDataFrame`, `SpatialLinesDataFrame` or
#' `SpatialPolygonsDataFrame`, depending on the specific geometry which is being read.
#'
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom rgeos readWKT
#' @importFrom sp CRS SpatialPolygonsDataFrame
#'
#' @examples
#'
dbReadSpatial <- function(conn, name, pkey = NULL, ...) {

  #   ____________________________________________________________________________
  #   Checks                                                                  ####

  if (length(name) == 2) {
    table_schema <- name[1]
    table_name   <- name[2]
  } else if (length(name) == 1) {
    table_schema <- "public"
    table_name   <- name[1]
  } else {
    stop("\"name\" must be a 1 or 2 character length variable.")
  }

  #   ____________________________________________________________________________
  #   Retrieve geometry column                                                ####

  meta_geom <- DBI::dbGetQuery(conn, paste0("SELECT * FROM geometry_columns ", "WHERE f_table_schema='",
                                            table_schema, "' ", "AND f_table_name = '", table_name, "';"))
  geom      <- meta_geom$f_geometry_column
  data_crs  <- sp::CRS(paste0("+init=epsg:", meta_geom$srid))

  #   ____________________________________________________________________________
  #    Retrieve column names and pkey                                         ####

  table_type <- DBI::dbGetQuery(conn, paste0("SELECT table_type FROM information_schema.tables ", "WHERE table_schema='",
                                             table_schema, "' ", "AND table_name = '", paste(name, collapse = "."), "';"))$table_type
  if (is.null(table_type)) {
    table_type   <- "MATERIALIZED VIEW OR MISSING"
  }

  if (table_type == "BASE TABLE") {
    meta_columns <- DBI::dbGetQuery(conn, paste0("SELECT a.attname, i.indisprimary FROM pg_index i ",
                                                 "JOIN pg_attribute a ON a.attrelid = i.indrelid ", "AND a.attnum = ANY(i.indkey) ", "WHERE i.indrelid = '",
                                                 paste(name, collapse = "."), "'::regclass;"))
    column_names <- meta_columns$attname[-match(geom, meta_columns$attname)]
    pkey         <- meta_columns[meta_columns$indisprimary, "attname"]
  } else {
    column_names <- suppressWarnings(names(DBI::dbGetQuery(conn, paste0("SELECT * FROM ", paste(name,
                                                                                                collapse = "."), " LIMIT 1"))))
    column_names <- column_names[-match(geom, column_names)]
  }
  if (is.null(pkey))
    stop("\"pkey\" value was not provided and no primary keys was found.")

  #   ____________________________________________________________________________
  #   retrieve data                                                           ####

  data_str <- DBI::dbGetQuery(conn, paste0("SELECT ", paste(column_names, collapse = ", "), ", ST_AsText(",
                                           geom, ") AS geom_t ", "FROM ", paste(name, collapse = "."), ";"))

  #   ____________________________________________________________________________
  #  Reshape and return data                                                            ####

  data_list <- list()
  for (i in 1:nrow(data_str)) {
    data_list[[i]] <- rgeos::readWKT(data_str$geom_t[i], data_str[[pkey]][i], data_crs)
  }
  data_sp            <- do.call(rbind, data_list)
  rownames(data_str) <- data_str[[pkey]]
  data_sp            <- sp::SpatialPolygonsDataFrame(data_sp, data_str[, column_names])

  return(data_sp)

}
