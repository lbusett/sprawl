#' @title Write a `sf` or `sp` to a ESRI shapefil
#' @description Wrapper on  `sf::st_write` for writing an `sf` or `sp` object to
#'  disk as an ESRI shapefile
#' @param out_obj `character` object to be written to the shapefile. Must be a
#'   valid `*sp` or `sf` object
#' @param out_file  `character` output file name
#' @param overwrite `logical` if TRUE, output file will be overwritten if existing,
#'   Default: FALSE
#' @param verbose `logical` if TRUE, provide messages on processing, Default: TRUE
#' @param encoding  `character` encoding to be used to write the DBF of the
#'   shapefile, Default: "UTF-8"
#' @param create_dir `logical`, if TRUE and the folder of out_file doesn't exist,
#'   then the full folder tree up to out_file is created, Default: FALSE
#'   (use with caution !)
#' @param ...  any other arguments to be passed to `sf::st_write`
#' @return NULL
#' @importFrom rgdal setCPLConfigOption
#' @importFrom sf st_write
#' @importFrom methods as
#' @importFrom tools file_ext file_path_sans_ext
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
#' out_file  = tempfile(pattern = "test", tmpdir = tempdir(), fileext = ".shp")
#' mysp_object = points
#' write_shape(mysp_object, out_file, overwrite = TRUE)
#'}
write_shape <- function(out_obj,
                       out_file,
                       overwrite  = FALSE,
                       verbose    = FALSE,
                       encoding   = "UTF-8",
                       create_dir = FALSE,
                       ... ) {

  rgdal::setCPLConfigOption("SHAPE_ENCODING", encoding)

  if (file.exists(out_file) & overwrite == FALSE) {
    stop(glue::glue(
      "write_shape --> Shapefile already exists. Aborting ! Set ",
      "`overwrite = TRUE` to allow overwriting."))
  }

  if (!dir.exists(dirname(out_file)) & create_dir == FALSE) {
    stop(glue::glue(
      "write_shape --> Output folder doesn't exist on your system. Either ",
      "create it beforehand with `dir.create` or set the `create_dir` ",
      "argument to `TRUE`"))
  }

  if (!dir.exists(dirname(out_file)) & create_dir == TRUE) {
    dir.create(dirname(out_file), recursive = TRUE)
  }

  if (tools::file_ext(out_file) != "shp") {
    out_file <- paste0(tools::file_path_sans_ext(out_file), ".shp")
  }

  intype <- get_spatype(out_obj)
  if (intype == "spobject") {
    out_obj <- methods::as(out_obj, "sf")
    intype  <- "sfobject"
  }
  if (intype == "sfobject") {

    sf::write_sf(obj = out_obj,
                 dsn           = out_file,
                 layer         = basename(out_file),
                 driver        = "ESRI Shapefile",
                 delete_layer  = overwrite,
                 quiet         = !verbose,
                 layer_options = ifelse(is.null(encoding),
                                        "",
                                        paste0("ENCODING=", encoding)),
                 ...)
  } else {
    stop("input object is NOT a valid `*sf` or `*sp` object. Aborting !")
  }
}

