#' @title write a spatial table to a PostGIS db table
#' @description write spatial table to a PostGIS db table (simple wrapper around
#'   `sf::st_write_db` with automatic casting to `sf` objects and argument
#'   checks)
#' @param in_vect either an "sf" or "sp" R object, or the full path of
#'   a valid vector file
#' @param con a `PostgreSQLConnection` object to a PostGIS DB, created by a call to
#'  `DBI::DBI::dbConnect(PostgreSQL(),....`
#' @param table_name `character` name of the table to be used for storage in the db
#' @param schema `character` name of the schema to be used for storage in the db
#' @param make_valid `logical` If FALSE, the function aborts in case invalid
#'    geometries are found. If TRUE, an automatic correction based on
#'    `lwgeom::st_make_valid` is attempted, Default: FALSE
#' @param drop `logical` indicating if existing tables should be overwrittern,
#'  Default: FALSE
#' @param row.names `logical` indicating if an additional row containing row numbers,
#'  should be added to the DB table, Default: FALSE
#' @return The function is called for its side effects
#' @seealso `sf::st_write_db` `lwgeom::st_make_valid` `sf::st_is_valid`
#' @examples
#' \dontrun{
#' # establish a connection with a postGIS DB
#' con <- DBI::dbConnect(PostgreSQL(),
#'                      host = "xx.xxx.xx.xx", dbname = "xxxxx", user = "xxxxx",
#'                      password = "xxxxx", port = 5432)
#'> con
#'<PostgreSQLConnection>
#'
#'  # write a vector object in a table of the DB
#'  write_vector_to_pgres(in_vect,
#'                   con        = con,
#'                   table_name = "my_table",
#'                   schema     = "public",
#'                   drop       = TRUE)
#' }
#' @rdname write_vector_to_pgres
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom sf st_write_db
#' @importFrom testthat expect_is
#' @importFrom DBI dbGetInfo

write_vector_to_pgres <- function(in_vect,
                                  con,
                                  table_name,
                                  schema     = "public",
                                  make_valid = FALSE,
                                  drop       = FALSE,
                                  row.names  = FALSE,
                                  verbose    = TRUE) {

  call <- match.call()
  if (verbose) {
    message("write_vector_to_pgres -->  Writing ", call[2], " to table `", call[3],
            "` of schema `", call[4], "` in db `",
            DBI::dbGetInfo(con)$dbname, "`")
  }

  testthat::expect_is(table_name, "character")
  testthat::expect_is(schema, "character")
  in_vect <- cast_vect(in_vect, "sfobject")

  # check for validity of geometries before pushing to the DB ----

  valid <- suppressWarnings(sf::st_is_valid(in_vect))

  if (!all(valid)) {
    if (make_valid) {
      message("write_vector_to_pgres --> The following geometries are invalid: ",
              format(as.character(which(valid == FALSE))),
              "\n Performing automatic correction!")
      orig_geom_type <- unique(st_dimension(st_geometry(in_vect)))
      if (orig_geom_type == 2) {
        orig_geom_type = "POLYGON"
      } else {
          stop("This function currently only supports polygon vectors. Aborting!")
        }

        in_vect <- suppressWarnings(lwgeom::st_make_valid(in_vect)) %>%
        # ensure to keep original type!
        sf::st_collection_extract(orig_geom_type)

    } else {
      stop("write_vector_to_pgres --> The following geometries are invalid: ",
           format(as.character(which(valid == FALSE))),
           "\n Use `make_valid = TRUE` to attempt an automatic correction!")
    }
  }

  # Push table to DB ----

  sf::st_write_db(con,
                  obj = in_vect,
                  table = c(schema, table_name),
                  drop = drop, row.names = row.names)
}
