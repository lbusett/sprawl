get_projstring <- function(object) {

  obj_type <- check_spatype(object)

  projstring <- dplyr::case_when(
    obj_type == "none" ~ stop("get_projstring --> ", object, "is not a valid vector or
                              raster object or filename. Aborting !"),
    obj_type == "spobject"   ~ "a",
    obj_type == "sfobject"   ~ "a",
    obj_type == "vectfile"   ~ "a",
    obj_type == "rastobject" ~ "a",
    obj_type == "rastfile"   ~ "a"

  )

}


get_projstring  <- function(object) {
  UseMethod("get_projstring")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

get_projstring.default   <- function(object) {
  stop("get_projstring --> ", object, "is not a valid vector or raster object or filename.
       Aborting !")
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

get_projstring.character <- function(object) {
  if (file.exists(object)) {
    vecttry <- suppressWarnings(try(gdalUtils::ogrinfo(object, al = TRUE, so = TRUE), silent = TRUE))
    if (is.null(attr(vecttry, "status"))) {
      as.character(gdalUtils::gdalsrsinfo(object, as.CRS = TRUE))
    } else {
      rastry  <- suppressWarnings(try(gdalUtils::gdalinfo(object), silent = TRUE))
      if (is.null(attr(rastry, "status"))) {
         as.character(gdalUtils::gdalsrsinfo(object, as.CRS = TRUE))
      } else {
        stop("get_projstring --> ", object, "is not a valid vector or raster object or filename.
       Aborting !")
      }
    }
  } else {
    stop("get_projstring --> ", object, "is not a valid vector or raster object or filename.
       Aborting !")
  }
}

#   ____________________________________________________________________________
#   Method for "Raster"                                                     ####

get_projstring.Raster <- function(object) {
  "rastobject"
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

get_projstring.sf     <- function(object) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                         ####

get_projstring.sfc    <- function(object) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

get_projstring.Spatial   <- function(object) {
  "spobject"
}


