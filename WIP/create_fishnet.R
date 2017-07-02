#' create_fishnet_old
#' @description funtion to create a raster and/or polygon fishnet of specified resolution over a
#' a specified spatial extent. If both in_ext and crop_ext are declared, the fishnet is created
#' according to in_ext (that is, the origin depends from in_ext), and then cropped on crop_ext (this
#' allows for example to create a fishnet based on a given raster/resolution, and then to automatically
#' crop it on a desired extgent without "moving" the corners of the cells ")
#'
#' @param in_ext        object of "Extent" class Extent on to which the grid has to be created
#' @param crop_ext      object of "Extent" class Extent on to which the crfeated grid has to be cropped
#' @param cellsize      numeric Dimensions of the desired cells
#' @param in_proj       character proj4string projection of the desired fishnet
#' @param out_raster    logical if TRUE, then a raster file is given as output. Defaults to TRUE
#' @param out_shape     logical if TRUE, then a shape file is given as output. Defaults to FALSE
#' @param out_rastfile  character (optional) name of the output raster file
#' @param out_shapefile character (optional) name of the output shape file
#' @param overw
#'
#' @importFrom sp GridTopology SpatialGridDataFrame CRS
#' @importFrom raster setValues writeRaster crop brick
#' @importFrom rgdal readOGR writeOGR

#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'   # create a fishnet over an input raster file and save it as a shapefile
#'   file_in <- "/somedir/somefile.tif"
#'   inrast  <- raster(file_in)
#'
#'   ext_rast <- extent(inrast)
#'   csize    <- res(in_mask)[1]
#'
#'   out_file <- "/somedir/somefile.shp"
#'   create_fishnet(ext_rast, cellsize = csize, out_shape = TRUE, out_shapefile = out_file,
#'          overw = TRUE, out_raster = FALSE)
#'
#' }

create_fishnet_old <- function(in_ext, cellsize, in_proj, out_raster = TRUE, out_shape = FALSE,
                           overw = FALSE, out_shapefile = NULL, crop_ext = NULL,
                           out_rastfile = NULL, pypath = NULL) {

  # dir.create(dirname(out_rastfile), recursive = TRUE, showWarnings = FALSE)
  if (is.null(cellsize)){
    stop("cellsize not specified !")
  }

  if (class(in_ext) != "Extent"){
    stop("in_ext must be an object of class: Extent")
  }

  if (!is.null(crop_ext) & class(crop_ext) != "Extent") {
    stop('crop_ext must be an object of class: Extent')
  }

  if (is.null(cellsize)){
    stop('error')
  }

  cs <- c(cellsize,cellsize)  # cell size.
  cc <- c(in_ext@xmin,in_ext@ymin) + (cs/2)   # corner of the grid. Since extent of a spatial object in R is defined
  # by centroids, we need to move by half pixel
  # compute number of cells per direction
  cd     <- ceiling(c(((in_ext@xmax - in_ext@xmin)/cs[1]),((in_ext@ymax - in_ext@ymin)/cs[2]))) - 1
  # construct grid topology
  grd    <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)   # Define grd characteristics
  #transform to spatial grid
  sp_grd <- SpatialGridDataFrame(grd,
                                 data = data.frame(id = seq(1,(prod(cd)),1)),  # ids are numbers between 1 and ns*nl
                                 proj4string = CRS(in_proj))

  # create RASTER ouput if required
  if (out_raster) {

    if (is.null(out_rastfile)) {out_rastfile <- tempfile("tempshp_", tempdir(), ".tif")}
    message("Writing Polygon Grid to: ", out_shapefile, " - Please Wait !")

    out_raster <- raster(sp_grd)
    setValues(out_raster, sp_grd@data$id)

    if (!is.null(crop_ext)) {
      out_raster <- crop(out_raster, crop_ext)
      out_raster[] <- seq(1, dim(out_raster)[1]*dim(out_raster)[2],1)
    }

    writeRaster(out_raster, out_rastfile, overwrite = TRUE)

  }

  # create shapefile ouput if required
  if (out_shape) {

    if (is.null(out_shapefile)) {out_shapefile <- tempfile("tempshp_", tempdir(), ".shp")}
    message("Writing Polygon Grid to: ", out_shapefile, " - Please Wait !")

    if (!is.null(crop_ext)) {
      sp_grd <- crop(brick(sp_grd), crop_ext)
    } else {
      sp_grd <- raster(sp_grd)
    }

    # Use routine "gdal_polygonizeR" to write the fishnet as a shapefile
    gdal_polygonizeR(sp_grd, readpoly = F, outshape = out_shapefile, overwrite = overw, pypath = pypath)

  }
}
