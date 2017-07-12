#' @title create_fishnet
#' @description funtion to create a raster and/or polygon fishnet of specified resolution over a
#' a specified spatial extent. If both in_ext and crop_ext are declared, the fishnet is created
#' according to in_ext (that is, the origin depends from in_ext), and then cropped on crop_ext (this
#' allows for example to create a fishnet based on a given raster/resolution, and then to automatically
#' crop it on a desired extgent without "moving" the corners of the cells ")
#' @param in_obj  object of `sf` or `raster*` class from which the fishnet needs to be derived. The fishnet is
#' built from the corner of `in_obj`, with a resolution equal to `cellsize`
#' @param cellsize numeric Dimensions of the desired cells
#' @param out_raster character (optional) name of the output raster file. If NULL the grid is NOT
#'  saved in a raster file Default: NULL
#' @param out_shape character (optional) name of the output shape file. If NULL the grid is NOT
#'  saved in a shapefile,  Default: NULL
#' @param overwrite `logical` If TRUE, existing files are overwritten, Default: FALSE
#' @param crop_layer (optional) object of `Extent`. If not null, the fishnet extent will be cropped on
#' this extent, without "moving" the nodes of the grid (this is useful to crop a grid created on
#' the basis of a different raster coordinates on top of a different raster, Default: NULL
#' @param return_sp `logical`, If true, the grid is returned to the caller as a `SpatialGridDataFrame ,
#' Default: FALSE
#' @param pypath path to `gdal_polygonize.py`. If NULL, the path is searched on the user's machine,
#' Default: NULL
#' @return if return_sp == TRUE, returns the grid as a SpatialGridDataFrame
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # create a fishnet over an input raster file and save it as a shapefile
#'   file_in <- "/somedir/somefile.tif"
#'   inrast  <- raster(file_in)
#'
#'   ext_rast <- extent(inrast)
#'   csize    <- res(inrast)[1]
#'
#'   out_file <- "/somedir/somefile.shp"
#'   create_fishnet(ext_rast, cellsize = csize, out_shape = TRUE, out_shapefile = out_file,
#'          overw = TRUE, out_raster = FALSE)
#'  }
#'  }
#' @rdname create_fishnet
#' @export
#' @importFrom raster raster crop extent mask writeRaster
#' @importFrom sf st_as_sf st_set_crs st_bbox st_crs st_transform st_buffer st_combine
#' @importFrom sp proj4string GridTopology SpatialGridDataFrame
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel

create_fishnet <- function(in_obj,
                           cellsize,
                           crop_layer = NULL,
                           out_raster = NULL,
                           out_shape  = NULL,
                           overwrite  = FALSE,
                           return_sp  = TRUE,
                           pypath     = NULL) {

  #   ____________________________________________________________________________
  #   Check the arguments                                                     ####
  #

  # checks on pypath
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }

  # checks on out_shape
  if (!is.null(out_shape)) {
    if (file.exists(out_shape)) {
      if (overwrite == TRUE) {
        unlink(paste(out_shape, c('shp', 'shx', 'dbf')))
      } else {
        stop("writeshape --> Shapefile already exists. Aborting ! Set `overwrite = TRUE` to allow overwriting.")
      }
    }
  }

  if (!file.exists(pypath)) {
    stop("create_fishnet --> gdal_polygonize.py was not found ! please check that it is available on your system
           and specify its position through the `py_path` argument ! Aborting !")
  }

  # checks on in_obj
  if (check_spatype(in_obj) == "spobject") {
    in_obj <- st_as_sf(in_obj)
  }
  if (check_spatype(in_obj) == "rastobject") {
    if (is.null(cellsize)) {
      warning("cellsize not specified. It will be derived from the `in_obj` raster")
      cellsize <- res(in_obj)[1]
    }
    proj   <- sp::proj4string(in_obj)
    in_obj <- sf::st_as_sf(as(extent(in_obj), 'SpatialPolygons')) %>%
      sf::st_set_crs(proj)
  }

  if (inherits(in_obj, "sf") == FALSE) {
    stop("`in_obj` must be an `sp`, `sf` or `*raster` object ! Aborting !")
  }

  # checks on crop_layer
  if (check_spatype(crop_layer) == "spobject") {
    crop_layer <- sf::st_as_sf(crop_layer)
  }
  if (check_spatype(crop_layer) == "rastobject") {
    proj   <- sp::proj4string(in_obj)
    in_obj <- sf::st_as_sf(as(extent(crop_layer), 'SpatialPolygons')) %>%
      sf::st_set_crs(proj)
  }

  if (!is.null(crop_layer) & inherits(crop_layer, "sfc")) {
    stop("`crop_layer` must be an `sp`, `sf` or `*raster` object ! Aborting !")
  }

  # checks on cellsize
  if (is.null(cellsize)) {
    stop("cellsize not specified ! Aborting !")
  }

  #   ____________________________________________________________________________
  #   retrieve information from the in_obj and crop_layer inputs              ####

  in_bbox   <- sf::st_bbox(in_obj)
  in_proj   <- sf::st_crs(in_bbox)$proj4string

  if (!is.null(crop_layer)) {
    crop_ext  <- sf::st_bbox(crop_layer)
    crop_proj <- sf::st_crs(crop_ext)$proj4string
    #   if projection of crop_layer not equal to in_object, reproject the extent####
    if (crop_proj != in_proj) {crop_layer <- sf::st_transform(crop_layer, in_proj)}
  }

  #   ____________________________________________________________________________
  #   Build a spatialgridDataFrame based on the input Extent and resoulution  ####
  #
  cs <- c(cellsize,cellsize)  # cell size.
  # Identidfy the corner of the grid. Since extent of a spatial object in R is defined
  # by centroids, we need to move by half pixel
  cc <- c(in_bbox[1], in_bbox[2]) + (cs/2)
  # Compute number of cells per direction
  cd     <- ceiling(c(((in_bbox[3] - in_bbox[1])/cs[1]),((in_bbox[4] - in_bbox[2])/cs[2])))
  # Build grid topology
  grd    <- sp::GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)   # Define grd characteristics
  # Create a SpatialGridDataFrame. ids are numbers between 1 and ns*nl
  sp_grd <- sp::SpatialGridDataFrame(grd,
                                     data = data.frame(id = seq(1,(prod(cd)),1)),
                                     proj4string = in_proj)

  #   ____________________________________________________________________________
  #   create a temporary raster from the sp_grd                              ####

  out_rst   <- raster::raster(sp_grd)
  out_rst[] <- seq(1, dim(out_rst)[1]*dim(out_rst)[2],1)
  gc()
  # if crop layer available, crop and mask the temporary raster on it
  if (!is.null(crop_layer)) {
    message("create_fishnet --> Cropping and masking the fishnet on `crop_layer`")
    # out_rst <- raster::crop(out_rst, raster::extent(crop_ext[c(1,3,2,4)]))
    # cropper <- as(sf::st_buffer(sf::st_combine(crop_layer), cellsize), "Spatial")
    # gc()

    out_rst <- maskrast(out_rst, crop_layer, buffer = res(out_rst)[1])
  }
  # assign the ids
  out_rastfile <- tempfile("tempshp_", tempdir(), ".tif")
  raster::writeRaster(out_rst, out_rastfile, overwrite = TRUE)
  gc()

  #   ____________________________________________________________________________
  #   if raster out selected, copy the raster fishnet to the path given       ####

  if (!is.null(out_raster)) {
    message("create_fishnet --> Writing Polygon Grid to: ", out_rastfile, " - Please Wait !")
    save_file <- try({
      dir.create(dirname(out_rastfile), recursive = T, showWarnings = FALSE)
      file.copy(out_rastfile, out_raster)
    })

    if (class(save_file) == "try-error") {
      stop("writing to ", save_file, " failed. Aborting !")
    }
  }

  #   ____________________________________________________________________________
  #   create shapefile ouput (if required)                                      ####

  if (!is.null(return_sp)) {

    if (is.null(out_shape)) out_shape = tempfile()
    #   ____________________________________________________________________________
    #   launch gdal_polygonize
    ####
    message("create_fishnet -> Writing Polygon Grid to: ", out_shape, " - Please Wait !")
    system2('python', args = paste(pypath,
                                   out_rastfile,
                                   "-f 'ESRI Shapefile'",
                                   out_shape,
                                   "-fieldname id", sep = " "), stdout = NULL)
  }

  if (return_sp) {
    message("create_fishnet -> Reading Polygon Grid ", out_shape, " to R - Please Wait !")
    outdata <- readshape(out_shape)
    return(outdata)
  }
}
