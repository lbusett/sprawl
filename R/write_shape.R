#' @title Write a `sf` or `sp` to a ESRI shapefile
#' @description Wrapper on  `sf::st_write` for writing an `sf` or `sp` object to
#'  disk as an ESRI shapefile
#' @param in_object `character` object to be written to the shapefile. Must be a
#'   valid `*sp` or `sf` object
#' @param out_filename  `character` output file name
#' @param overwrite `logical` if TRUE, output file will be overwritten if existing,
#'   Default: FALSE
#' @param verbose `logical` if TRUE, provide messages on processing, Default: TRUE
#' @param encoding  `character` encoding to be used to write the DBF of the
#'   shapefile, Default: "UTF-8"
#' @param create_dir `logical`, if TRUE and the folder of out_filename doesn't exist,
#'   then the full folder tree up to out_filename is created, Default: FALSE
#'   (use with caution !)
#' @param ...  any other arguments to be passed to `sf::st_write`
#' @return NULL
#' @importFrom rgdal setCPLConfigOption
#' @importFrom sf st_write
#' @importFrom methods as
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom checkmate expect_access
#' @export
#' @examples \dontrun{
#' # build a spatialpoints data frame
#' pts                <- cbind(1:5, 1:5)
#' dimnames(pts)[[1]] <- letters[1:5]
#' df                 <-  data.frame(a = 1:5)
#' row.names(df)      <- letters[5:1]
#' points             <- sp::SpatialPointsDataFrame(pts, df, match.ID = TRUE)
#'
#' # Save it to a shape file
#' out_filename  = tempfile(pattern = "test", tmpdir = tempdir(),
#'                          fileext = ".shp")
#' mysp_object = points
#' write_shape(mysp_object, out_filename, overwrite = TRUE)
#'}
write_shape <- function(in_object,
                        out_filename,
                        overwrite  = FALSE,
                        verbose    = FALSE,
                        encoding   = "UTF-8",
                        create_dir = FALSE,
                        ... ) {

  rgdal::setCPLConfigOption("SHAPE_ENCODING", encoding)

  if (file.exists(out_filename) & overwrite == FALSE) {
    stop(glue::glue(
      "write_shape --> Shapefile already exists. Aborting ! Set ",
      "`overwrite = TRUE` to allow overwriting."))
  }

  if (!dir.exists(dirname(out_filename)) & create_dir == FALSE) {
    stop(glue::glue(
      "write_shape --> Output folder doesn't exist on your system. Either ",
      "create it beforehand with `dir.create` or set the `create_dir` ",
      "argument to `TRUE`"))
  }

  make_folder(out_filename)

  checkmate::expect_access(dirname(out_filename), "w")

  if (tools::file_ext(out_filename) != "shp") {
    warning("write_shape --> Output file extension is not \".shp\". It will be",
            "changed automatically")
    out_filename <- paste0(tools::file_path_sans_ext(out_filename), ".shp")
  }

  intype <- get_vectype(in_object)
  if (intype == "spobject") {
    in_object <- methods::as(in_object, "sf")
    intype  <- "sfobject"
  }
  sf::write_sf(obj           = in_object,
               dsn           = out_filename,
               layer         = basename(out_filename),
               driver        = "ESRI Shapefile",
               delete_layer  = overwrite,
               quiet         = !verbose,
               layer_options = ifelse(is.null(encoding),
                                      "",
                                      paste0("ENCODING=", encoding)),
               ...)
}
