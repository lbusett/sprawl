#' @title download vecor of administrative boundaries
#' @description function to download administrative boundaries data from the gadm archive, starting
#' from a country name or ISO code, and the level of desired administrtive units
#' @param iso `character` iso name or 2/3 digits code of the country
#' @param level `numeric` level of administrative units returned, default: 0 (Country Level)
#' @param path `character`
#' @param makefold `character`
#' @return SpatialPolygonsDataFrame containing the desired data
#' @details the function is a simple wrapper to the raster::getData function, with a-priori checks
#' on country name or ISO code
#' @examples
#' \dontrun{
#'  library(sprawl)
#'  library(sp)
#'  ita_boundaries <- get_boundaries("Italy", 0)
#'  ita_region_boundaries <- get_boundaries("ITA", 1)
#'  plot(ita_region_boundaries)
#'  }

#' @seealso
#'  \code{\link[raster]{getData}}
#' @rdname get_boundaries
#' @export
#' @importFrom raster getData
get_boundaries <- function(iso,
                           level    = 1,
                           path     = NULL,
                           makefold = TRUE) {

  #   ____________________________________________________________________________
  #   check if level is numeric and iso is a recognized country name or ISO code ####

  if (!is.numeric(level)) stop("get_boundaries --> level must be numeric. Aborting !")

  data(iso3166, package = "maps", envir = environment())
  match_name <- match(iso, iso3166$ISOname)
  if (!is.na(match_name)) {
    iso_code <- maps::iso3166$a3[match_name]
  } else {
    match_code_i3 <- match(iso, iso3166$a3)
    if (!is.na(match_code_i3)) {
      iso_code <- iso
    } else {
      match_code_i2 <- match(iso, iso3166$a2)
      if (!is.na(match_code_i2)) {
        iso_code <- iso3166$a2[match_code_i2]
      } else {
        stop("get_boundaries --> Unrecognized ISO code or country name. Aborting !")
      }
    }
  }

  if (is.null(path)) {
    path = tempdir()
  } else {
    if (!dir.exists(path)) {
      if (makefold) {
        builddir <- try(dir.create(path, recursive = TRUE, showWarnings = FALSE))
        if (class(builddir == "try-error")) stop("get_boundaries --> Unable to create the ", path, "folder. Please
                                               check your inputs and verify file system permissions. Aborting")
      } else {
        stop("get_boundaries --> `path` folder doesn't exist on your system. Either create it beforehand, or
set `makefold` to TRUE. Aborting")
      }
    }
  }
  #   ____________________________________________________________________________
  #   Download the data                                                       ####

  adm <- suppressWarnings(try(raster::getData("GADM",
                                              country = iso_code,
                                              level   = level,
                                              path    = path),
                              silent = TRUE))
  if (class(adm) == "try-error") {
    stop("get_boundaries --> Download failed. It is possible that the level of data you are trying to download
         is not available. Please check at: `http://gadm.org/country`. Aborting !")
  }
  return(adm)
}
