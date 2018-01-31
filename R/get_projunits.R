#' @title Retrieve projection units
#' @description Function to retrieve projection units starting from a
#'   valid proj4string or CRS
#' @param proj4string valid proj4string or object of class `sp::CRS`
#' @return `character` measure units of the provided proj4string (e.g., "m",
#'   "dec.degrees", ...)
#' @examples
#'
#'   # latlong projection
#'   proj_string <- "+init=epsg:4326"
#'   get_projunits(proj_string)
#'
#'   # metric projection
#'   proj_string <- "+init=epsg:3857"
#'   get_projunits(proj_string)
#'
#' @rdname get_projunits
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom sp CRS
#' @importFrom stringr str_split str_trim
get_projunits <- function(proj4string) {

  #   __________________________________________________________________________
  #   Check arguments                                                       ####

  assertthat::assert_that(
    class(proj4string) == "character" | class(proj4string) == "CRS",
    msg = "proj4string is not a character or CRS object. Aborting!")

  #   __________________________________________________________________________
  #   Convert to CRS if needed                                              ####

  if (class(proj4string) == "character") {
    proj4string <- try(sp::CRS(proj4string), silent = TRUE)
    if (class(proj4string) == "try-error"){
      warning("get_projunits --> Unable to retrieve units !")
      return(NA)
    }
  }
  #   ________________________________________________________________________
  #   Find projection type                                                ####
  parse_proj <- stringr::str_split(proj4string@projargs, "\\+",
                                   simplify = TRUE) %>%
    stringr::str_split(pattern = "proj=", simplify = TRUE)

  proj_row <- which(parse_proj[,2] != "")
  if (length(proj_row) == 1) {
    proj_type <- parse_proj[proj_row, 2] %>%
      stringr::str_trim()
    if (proj_type == "longlat") {
      return("dec.degrees")

    #   ________________________________________________________________________
    #   if ! latlong proj_type, findunits                                 ####
    } else {
      parse_units <- stringr::str_split(proj4string@projargs, "\\+",
                                        simplify = TRUE) %>%
        stringr::str_split(pattern = "units=", simplify = TRUE)
      if (dim(parse_units)[2] != 1 ) {
        units_row <- which(parse_units[,2] != "")
        if (length(units_row) == 1) {
          return(stringr::str_trim(parse_units[units_row, 2]))
        }
      }
    }
  }

  warning("get_projunits --> Unable to retrieve units !")
  return("unknown")
}
