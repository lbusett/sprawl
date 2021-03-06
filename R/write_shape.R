#' @title Write a `sf` or `sp` to a ESRI shapefile
#' @description Wrapper on  `sf::st_write` for writing an `sf` or `sp` object to
#'  disk as an ESRI shapefile
#' @param in_vect `character` object to be written to the shapefile. Must be a
#'   valid `*sp` or `sf` object
#' @param out_file  `character` output file name
#' @param overwrite `logical` if TRUE, output file will be overwritten if existing,
#'   Default: FALSE
#' @param verbose `logical` if TRUE, provide messages on processing, Default: TRUE
#' @param encoding  `character` encoding to be used to write the DBF of the
#'   shapefile, Default: "UTF-8"
#' @param create_dir `logical`, if TRUE and the folder of out_file does not exist,
#'   then the full folder tree up to out_file is created, Default: FALSE
#'   (use with caution !)
#' @param ...  any other arguments to be passed to `sf::st_write`
#' @return NULL
#' @importFrom rgdal setCPLConfigOption
#' @importFrom sf st_write
#' @importFrom methods as
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom checkmate expect_access
#' @export
#' @examples
#'
#' # build a spatialpoints data frame
#' pts                <- cbind(1:5, 1:5)
#' dimnames(pts)[[1]] <- letters[1:5]
#' df                 <-  data.frame(a = 1:5)
#' row.names(df)      <- letters[5:1]
#' points             <- sp::SpatialPointsDataFrame(pts, df, match.ID = TRUE)
#'
#' # Save it to a shape file
#' out_file  = tempfile(pattern = "test", tmpdir = tempdir(),
#'                          fileext = ".shp")
#' mysp_object = points
#' write_shape(mysp_object, out_file, overwrite = TRUE)
#'
#' # read the saved shape
#' in_data <- read_vect(out_file)
#' in_data
#'
#'
write_shape <- function(in_vect,
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
      "write_shape --> Output folder does not exist on your system. Either ",
      "create it beforehand with `dir.create` or set the `create_dir` ",
      "argument to `TRUE`"))
  }

  make_folder(out_file, type = "filename", verbose = FALSE)

  checkmate::expect_access(dirname(out_file), "w")

  if (tools::file_ext(out_file) != "shp") {
    warning("write_shape --> Output file extension is not \".shp\". It will be",
            " changed automatically")
    out_file <- paste0(tools::file_path_sans_ext(out_file), ".shp")
  }

  in_vect <- cast_vect(in_vect, "sfobject")

  # Automatically remove whitespaces from column names (similar to dot
  # replacement in st_write)

  if (length(wherespace <- grep(" ", names(in_vect))) > 0) {
    warning("write_shape --> White spaces in column names will be replaced by",
            "\"_\"!")
    names(in_vect) <- stringr::str_replace_all(names(in_vect), " ", "_")
  }

  # Automatically shorten column names before attempting to save


  if (any(nchar(names(in_vect)) > 10)) {
    warning("write_shape --> Column names are too long for saving to .shp",
            "\n They will be automatically abbreviated!")
  names(in_vect) <- stringr::str_replace_all(names(in_vect), " ", "_") %>%
                                    abbreviate(10)
  }

  sf::write_sf(obj           = in_vect,
               dsn           = out_file,
               layer         = basename(out_file),
               driver        = "ESRI Shapefile",
               delete_layer  = overwrite,
               quiet         = !verbose,
               layer_options = ifelse(is.null(encoding),
                                      "",
                                      paste0("ENCODING=", encoding)),
               ...)
  return(invisible(in_vect))
}
