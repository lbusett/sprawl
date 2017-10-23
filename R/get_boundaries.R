#' @title Download vector of administrative boundaries from GADM
#' @description Function to download administrative boundaries data from the gadm
#'   archive, starting from a country name or ISO code, and the level of desired
#'   administrative units. The function is a simple wrapper to the
#'   `raster::getData` function, with a-priori checks on country name or ISO code
#'   and automatic recasting to `sf` format.
#' @param iso `character` iso name or 2/3 digits code of the country
#' @param level `numeric` level of administrative units returned, Default: 0
#'   (Country Level)
#' @param path `character` folder where the "RData" file containing polygon
#'   boundaries should be saved, Default: `tempdir()`
#' @return `sf` object containing the desired data
#' @examples
#'
#'  library(sprawl)
#'  library(sf)
#'  ita_boundaries <- get_boundaries("Italy", 0)
#'  plot_vect(ita_boundaries)
#'
#'  ita_region_boundaries <- get_boundaries("ITA", 1)
#'  plot_vect(ita_region_boundaries, fill_var = "NAME_1")
#'
#'  lomb_boundaries <- ita_region_boundaries %>%
#'     dplyr::filter(NAME_1 == "Lombardia")
#'  plot_vect(lomb_boundaries)
#'
#' @seealso
#'  \code{\link[raster]{getData}}
#' @rdname get_boundaries
#' @export
#' @importFrom raster getData
#' @importFrom sf st_sf
#' @importFrom utils data
#'
get_boundaries <- function(iso,
                           level = 1,
                           path  = tempdir()) {

  call <- match.call()

  message("get_boundaries --> Downloading data for: ",
          deparse(substitute(call)$iso),
          ", Level: ", deparse(substitute(call)$level))

  #   __________________________________________________________________________
  #   check if level is numeric and iso is a recognized country name or ISO ####
  #   code

  if (!is.numeric(level)) stop(
    "get_boundaries --> `level` must be numeric. Aborting !"
  )

  iso3166 <- get(utils::data(iso3166, package = "maps", envir = environment()))
  match_name <- match(iso, iso3166$ISOname)
  if (!is.na(match_name)) {
    iso_code <- iso3166$a3[match_name]
  } else {
    match_code_i3 <- match(iso, iso3166$a3)
    if (!is.na(match_code_i3)) {
      iso_code <- iso
    } else {
      match_code_i2 <- match(iso, iso3166$a2)
      if (!is.na(match_code_i2)) {
        iso_code <- iso3166$a2[match_code_i2]
      } else {
        stop("get_boundaries --> Unrecognized ISO code or country name. Aborting !") #nolint
      }
    }
  }

  #   __________________________________________________________________________
  #   Download the data                                                     ####
  make_folder(path)
  adm <- suppressWarnings(try(raster::getData("GADM",
                                              country = iso_code,
                                              level   = level,
                                              path    = path),
                              silent = TRUE))

  if (class(adm) == "try-error") {
    stop("get_boundaries --> Download failed.\nIt is possible that the level
         of data you are trying to download is not available.\n",
         "Please check at: http://gadm.org/country. Aborting !")
  }
  return(sf::st_as_sf(adm))
}
