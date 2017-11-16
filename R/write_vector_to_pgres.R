#' @title write spatial table to a PostGIS db table
#' @description write spatial table to a PostGIS db table
#' @param vector_to_add either an "sf" or "sp" R object, or the full path of
#'   a valid vector file
#' @param con a `PostgreSQLConnection` object to a PostGIS DB, created by a call to
#'  `DBI::DBI::dbConnect(PostgreSQL(),....`
#' @param table_name `character` name of the table to be used for storage in the db
#' @param schema `character` name of the schema to be used for storage in the db
#' @param drop `logical` indicating if existing tables should be overwrittern,
#'  Default: FALSE
#' @return The function is called for its side effects
#' @details DETAILS
#' @examples
#' \dontrun{
#'con <- DBI::dbConnect(PostgreSQL(),
#'                      host = "xx.xxx.xx.xx", dbname = "xxxxx", user = "xxxxx",
#'                      password = "xxxxx", port = 5432)
#'> con
#'<PostgreSQLConnection>
#'
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

add_vector_to_pgres <- function(vector_to_add,
                                con,
                                table_name,
                                schema = "public",
                                drop = FALSE) {

  message("Adding table `", table_name, "` to schema `", schema, "` of db `",
          DBI::dbGetInfo(con)$dbname, "`")

  testthat::expect_is(table_name, "character")
  testthat::expect_is(schema, "character")
  in_vect <- cast_vect(vector_to_add, "sfobject")

  sf::st_write_db(con,
                  obj = in_vect,
                  table = c(schema, table_name),
                  drop = drop)
}
