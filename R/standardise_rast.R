#' @title Generate a standardised raster
#' @description This function can be used to process a raster in order to
#'  1) obtain standardised values (Z-scores) within polygons of a specified
#'      vector file or R spatial object, and/or
#'  2) replace values of the raster near to the borders of the polygons with
#'      local or global averages, and/or
#'  3) fill holes (NA values) with local or global averages.
#'
#'  Some aliases are set for specific functions (see details).
#' @details
#'  Some aliases are set for specific functions:
#'  - `zscore_rast()` computes the Z-score on the input raster
#'      (equivalent to `standardise_rast(method = "zscore")`);
#'  - `fixborders_rast()` can be used to replace border values
#'      (equivalent to `standardise_rast(method = "input", by_poly = TRUE)`);
#'  - `fillgaps_rast()` is used to fill gaps (NA values) without using
#'      polygon masks
#'      (equivalent to `standardise_rast(by_poly = FALSE, min_area = 0,
#'      method = "input", fill_na = TRUE, fill_borders = FALSE, buffer = 0,
#'      parallel = FALSE)`).
#'
#'  By default, input raster values are standardised basing on the averages
#'  and standard deviations computed within each polygon (it is possible to
#'  specify a buffer value for computing them setting the `buffer` argument,
#'  i.e. in order to exclude border values).
#'  Oterwhise, setting `by_poly = FALSE` these metrics are computed on the whole
#'  polygons (so raster values are simply linearly combined and masked on the
#'  polygon surface).

#' @param in_rast The input raster or path.
#' @param in_vect The vector file or object containing the polygons used as
#'  mask and as polygons within which computing the standardised values.
#' @param out_file The path of the output raster; if NULL (default),
#'  a RasterLayer is returned by the function).
#' @param by_poly Logical: if TRUE (default), input raster values are
#'  standardised basing on the averages and standard deviations computed
#'  within each polygon; if FALSE, they are computed on the whole
#'  polygons.
#' @param min_area Numeric: the minimum area (in squared metres) that polygons
#'  must have to be considered and included in the final raster; default is
#'  0 (all non empty polygons are considered). Notice that, if a value of
#'  `buffer` > 0 is set, the value of `min_area` applies also to buffered
#'  polygons.
#' @param method Character: the method used to standardise values.
#'  Accepted values are:
#'  * `"zscore"` (default): compute the Z-score (real standardisation);
#'  * `"center"`: center values over the average;
#'  * `"input"`: do not standardise values (this method makes sense only
#'      with `fill_method != "input"`, as a method to
#'      interpolate values of border or holes).
#' @param fill_method Character: the method used to fill buffered areas
#'  (if `buffer < 0`) and holes (if `fill_na = TRUE`).
#'  Accepted values are:
#'  * `"focal"` (default): a focal weight is applied to borders, so values
#'      near the border of buffers are expanded (see details in the function
#'      code);
#'  * `"average"`: the averaged value within each polygon buffer
#'      (corresponding to 0 with `method = "zscore"` or `"center"`)
#'      is repeaded;
#'  * `"source"`: standardised input values are maintained (but they do not
#'      concur to compute average and standard deviation values used to
#'      sdtandardise).
#' @param fill_na Logical: if TRUE (default), NA values within polygons are
#'  filled using the method selected with argument `fill_method`;
#'  if FALSE, they are letf to NA.
#' @param fill_borders Logical: if TRUE (default), values between the border of
#'  the polygons and the negative buffers around the polygons are filled using
#'  the method set with argument `fill_method`; if FALSE, they are left to NA.
#'  This argument makes sense only with `buffer < 0`.
#' @param buffer Numeric: the buffer (in metres) applied to polygons usd to
#'  compute average and standard deviation (default is 0).
#'  If `fill_borders = TRUE` (default) pixels which are outside this buffer are
#'  still computed in the final raster (with the method chosen with
#'  `fill_method` argument), but their input values do not concur to compute
#'  average and standard deviation. Notice that, in order to exclude values
#'  of borders, a negative value of `buffer` must be provided.
#' @param parallel Logical or integer: if TRUE, the algorythm used to compute
#'  output values is runned parallelised on single polygons and the number of
#'  cores is automatically determined (otherwise, an integer value can be
#'  provided in order to set this value manually); if FALSE (or 1), it is
#'  runned on a single core. This value is not considered if `by_poly = TRUE`
#'  (in this case, it is runned on a single core).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is to maintain each input format.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#'  Default is "DEFLATE".
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @return NULL if `out_file` is not NULL; the output RasterLayer otherwise.
#' @export
#' @author Luigi Ranghetti, phD (2018) <ranghetti.l@irea.cnr.it>
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom sf st_buffer st_area st_geometry st_sf st_union
#' @importFrom raster calc writeRaster values resample mosaic
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom jsonlite fromJSON
#' @importFrom rgdal GDALinfo writeGDAL
#' @importFrom gdalUtils gdalwarp gdal_rasterize
#' @importFrom methods as
#' @examples
#' \dontrun{
#' # These examples cannot be runned globally;
#' # a global example will be done in future.
#'
#' # Define input files and paths
#' projects_dir  <- "/mnt/projects" # 10.0.1.230
#' nrworking_dir <- "/mnt/nr_working" # 10.0.1.230
#' data_dir <- file.path(nrworking_dir,"luigi/data/s2tsp/180117_test_zscore/MSAVI")
#' raster_ex <- raster(file.path(data_dir,"S2B2A_20180104_022__MSAVI_10.tif"))
#' source(file.path(nrworking_dir,"luigi/code/satfarming/opencon_satfarming.R"))
#' appez <- st_read_db(
#'   con_satfarming,
#'   query = "SELECT * FROM bf_jolanda.appez WHERE anno = 2017;"
#' )
#'
#' # Example 1: negative big buffer, focal method, fill NA
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore1.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -30, min_area=1E4,
#'   parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "focal", fill_na = TRUE
#' )
#'
#' # Example 1.1: negative big buffer, focal method, do not fill NA
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore1_1.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -30, min_area=1E4,
#'   parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "focal", fill_na = FALSE
#' )
#'
#' # Example 2: negative buffer, average method
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -10, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "average"
#' )
#'
#' # Example 2.1: negative buffer, average method, fill_na=FALSE
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_1.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -10, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "average", fill_na = FALSE
#' )
#'
#' # Example 2.2: negative buffer, average method, do not fill borders
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_2.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -20, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "average", fill_borders = FALSE
#' )
#'
#' # Example 2.3: negative buffer, do not fill neither borders nor holes
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_3.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = -20, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "indifferent", fill_borders = FALSE, fill_na = FALSE
#' )
#'
#' # Example 3: no buffer, only fill holes
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore3.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = 0, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "focal"
#' )
#'
#' # Example 4: do not standardise nor buffer, only fill holes (average)
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore4.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = 0, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "input", fill_method = "average"
#' )
#'
#' # Example 5: positive (exagerated) buffer
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore5.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=TRUE, buffer = 250, min_area=1E4, parallel=TRUE, overwrite=TRUE,
#'   method = "zscore", fill_method = "focal"
#' )
#'
#' # Example 6: remove borders and holes, globally
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore6.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file, in_vect=appez[58:60,],
#'   by_poly=FALSE, buffer = -30, min_area=1E4, overwrite=TRUE,
#'   method = "zscore", fill_method = "focal"
#' )
#'
#' # Example 7: fill holes without masking over polygons
#' out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore7.tif"))
#' tmp <- standardise_rast(
#'   raster_ex, out_file,
#'   buffer = "indifferent", min_area=1E4, overwrite=TRUE,
#'   method = "input", fill_method = "focal"
#' )
#' }
#'
standardise_rast <- function(in_rast,
                             out_file = NA,
                             in_vect = NULL,
                             by_poly  = TRUE,
                             min_area = 0,
                             method = "zscore",
                             fill_method = "focal",
                             fill_na = TRUE,
                             fill_borders = TRUE,
                             buffer = 0,
                             parallel = TRUE,
                             format = NA,
                             compress = "DEFLATE",
                             overwrite = FALSE
) {

  # verify input raster
  in_rast_path <- cast_rast(in_rast, "rastfile")

  # verify input vect
  if (!is.null(in_vect)){
    in_vect <- cast_vect(in_vect, "sfobject")
  } else {
    in_vect <- sf::st_sf(data.frame(
      id = "All",
      geometry = as(get_extent(in_rast_path), "sfc_POLYGON")
    ))
    buffer <- 0 # buffering does not make sense on a whole raster
  }

  # check output format
  if (is.na(format)) {
    format <- suppressWarnings(attributes(GDALinfo(in_rast_path))[["driver"]])
  }
  gdal_formats <- fromJSON(
    # system.file("extdata","gdal_formats.json",package="fidolasen")
    readLines("https://raw.githubusercontent.com/ranghetti/fidolasen/master/inst/extdata/gdal_formats.json")
  )
  sel_driver <- gdal_formats[gdal_formats$name==format,]
  if (nrow(sel_driver)==0) {
    stop(paste0(
      "Format \"",format,"\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
    ))
  }

  # buffer if required
  if (buffer!=0) {
    in_vect_buf <- sf::st_buffer(in_vect, buffer)
  }

  # If not working by polygon, join all features
  # to perform clustering on whole image
  if (by_poly==FALSE) {
    message(paste0(
      "[",Sys.time(),"] ",
      "Dissolving polyons..."
    ))
    in_vect <- sf::st_union(in_vect)
    in_vect <- sf::st_sf(
      data.frame(id = "All",
                 geometry = sf::st_geometry(in_vect))
    )
    if (buffer!=0) {
      in_vect_buf <- sf::st_union(in_vect_buf)
      in_vect_buf <- sf::st_sf(
        data.frame(id = "All",
                   geometry = sf::st_geometry(in_vect_buf))
      )
    }
  }

  # Define a reduced version of fidolasen::gdal_warp checked only
  # for the specific needings of standardise_rast().
  # This because original gdal_warp does not mask in parallel mode;
  # moreover, fidolasen can not be set as a sprawl dependence, since fidolasen
  # depends from sprawl).
  # It can be improved (gdalUtils::gdalwarp is slower than gdalwarp from commandline).
  gdal_warp <- function(srcfiles, dstfiles, mask) {
    srcfile <- srcfiles[1]; dstfile <- dstfiles[1]
    sel_tr <- suppressWarnings(rgdal::GDALinfo(srcfile)[c("res.x","res.y")])
    gdalUtils::gdalwarp(
      srcfile, dstfile,
      cutline = mask, crop_to_cutline = TRUE,
      tr = sel_tr, tap = TRUE
    )
  }

  # set the function for the chosen method
  standardise <- switch(
    method,
    zscore = function(x,avg,std){(x-avg)/std},
    center = function(x,avg,std=NA){x-avg},
    input = function(x,avg=NA,std=NA){x},
    stop("Value of attribute \"method\" not recognised.")
  )

  # Computing the required n_cores
  n_cores <- if (parallel==FALSE | by_poly==FALSE | nrow(in_vect)==1) {
    1
  } else if (is.numeric(parallel)) {
    as.integer(parallel)
  } else {
    min(parallel::detectCores() - 1, 8) # use at most 8 cores
  }
  if (n_cores<=1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    by_poly <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
    parallel <- TRUE
  }

  # Cycle on single polygons
  message(paste0(
    "[",Sys.time(),"] ",
    "Starting to standardise raster",
    if (by_poly==TRUE) {" within each polygon"},
    "..."
  ))
  if (parallel) {
    cluster <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cluster)
  }
  rast_z_paths <- foreach(i = seq_along(in_vect[[1]]),
                          .packages = c("sf", "sprawl", "raster"),
                          .verbose = FALSE,
                          .combine = "rbind"
  ) %DO% {

    ## Create names of temporary files
    # vect: selected polygon
    sel_vect_path <- cast_vect(in_vect[i,], "vectfile")
    # vect: buffer on the selected polygon
    if (buffer!=0) {sel_vect_buf_path <- cast_vect(in_vect_buf[i,], "vectfile")}
    # rast_vect: rasterised polygon (used for masking)
    sel_rast_vect_path <- tempfile(fileext = "_vect.tif")
    # rast_poly: input raster used as reference (cropped on the border of the polygon)
    sel_rast_poly_path <- tempfile(fileext = "_poly.tif")
    # rast_buf: raster used to compute metrics (cropped on the border of the buffered polygon)
    sel_rast_buf_path <- tempfile(fileext = "_buf.tif")
    # rast_filled: raster with the values (originals and filled) to be cropped on the border of the polygon
    sel_rast_fil_path <- tempfile(fileext = "_fil.tif")
    # rast_crop: raster with the values to be standardised (cropped on the border of the polygon)
    sel_rast_crop_path <- tempfile(fileext = "_crop.tif")
    # rast_z: raster with the standardised values (cropped on the border of the polygon)
    sel_rast_z_path <- tempfile(fileext = "_z.tif")


    ## Checks on polygon area

    # Check that the area of the polygon is > min_area
    sel_vect <- cast_vect(sel_vect_path, "sfobject")
    if (st_area(st_geometry(sel_vect)) <= min_area * units::ud_units$m^2) {
      warning(paste0(
        "Polygon ",i," will not be considered, since it is smaller (",
        round(st_area(st_geometry(sel_vect))),
        " m²) than the minimum accepted area (",min_area," m²)."
      ))
      continue_sel_poly <- FALSE
    } else {
      continue_sel_poly <- TRUE # logical: if TRUE, the chunk in the foreach cycle
      # can continue computing the map of zscore; if FALSE, it does not continue
      # with the selected polygon. This is a workaround of using "break", which
      # does not work with "foreach" cycles.
    }

    # Check that the area of the buffer is > min_area
    if (continue_sel_poly & buffer<0) {
      sel_vect_buf <- cast_vect(sel_vect_buf_path, "sfobject")
      if (st_area(st_geometry(sel_vect_buf)) <= min_area * units::ud_units$m^2) {
        warning(paste0(
          "Polygon ",i," will not be considered, since its buffer is smaller (",
          round(st_area(st_geometry(sel_vect_buf))),
          " m²) than the minimum accepted area (",min_area," m²). ",
          "To consider it, try to use a higher value for \"buffer\" argument."
        ))
        continue_sel_poly <- FALSE
      } else {
        continue_sel_poly <- TRUE
      }
    }

    if (continue_sel_poly) { # see the note above


      ## 1. Generate raster covering only the buffer of the selected polygon

      # Crop values on the border of the polygon
      if (TRUE) { # TODO exclude cases in which this is not needed
        gdal_warp(in_rast_path, sel_rast_poly_path, mask=sel_vect_path)
        sel_rast_poly <- cast_rast(sel_rast_poly_path, "rastobject")
      }

      # Crop the values on the border of the buffer
      if (buffer!=0) {
        gdal_warp(in_rast_path, sel_rast_buf_path, mask=sel_vect_buf_path)
        sel_rast_buf <- cast_rast(sel_rast_buf_path, "rastobject")
      } else {
        sel_rast_buf <- sel_rast_poly
      }

      # In case of negative buffer, use a grid larger than the input,
      # as needed by the focal
      if (buffer<0) {
        sel_rast_buf <- resample(sel_rast_buf, sel_rast_poly, method="ngb")
      }


      ## 2. Compute the metrics required to standardise
      sel_rast_avg <- mean(values(sel_rast_buf), na.rm=TRUE)
      sel_rast_std <- sd(values(sel_rast_buf), na.rm=TRUE)


      ## 3. Manage the values of pixels between the border of the polygon and the buffer
      # TODO manage this case also with buffer>=0, as a method to fill gaps
      # Case 1 (negative buffer):
      # fill the values between the border of the polygon and the buffer
      # using the method chosen with fill_method

      sel_rast_fil <- sel_rast_buf
      if (buffer<0 & fill_borders == TRUE | fill_na == TRUE) {
        if (fill_method == "focal") {
          # rasterize in_vect (reference for the surface to be filled)
          file.copy(sel_rast_poly_path, sel_rast_vect_path)
          gdalUtils::gdal_rasterize(
            if (fill_borders==FALSE & buffer<0) {sel_vect_buf_path} else {sel_vect_path},
            sel_rast_vect_path,
            burn=1
          )
          sel_rast_vect <- cast_rast(sel_rast_vect_path, "rastobject")
          j <- 0
          # continue interpolating until all the pixels in the polygon are covered
          max_iter_n <- 100 # maximum number of iterations
          while (any(values(as.integer(!is.na(sel_rast_vect)) - as.integer(!is.na(sel_rast_fil))) == 1) &
                 j < max_iter_n) {
            j <- j+1
            # sel_w <- focalWeight(sel_rast_fil, -buffer*sqrt(j), "circle")
            sel_w <- focalWeight(sel_rast_fil, mean(res(sel_rast_fil))*sqrt(j), "circle") # 1 pixel per time
            sel_w <- sel_w / max(sel_w) # adjustment required by focal to fix the problem with na.rm=TRUE
            sel_rast_fil <- focal(
              sel_rast_fil,
              fun = function(x){weighted.mean(x,sel_w,na.rm=TRUE)},
              w = sel_w, pad=TRUE, NAonly=TRUE
            )
          }
          if (j == max_iter_n) {
            warning(paste0(
              "Borders and/or gaps have not been completely filled ",
              "(maximum number of iterations was reached)."
            ))
          }

        } else if (fill_method == "average") {
          # fill NA and mask outside polygon
          sel_rast_fil[is.na(sel_rast_fil)] <- sel_rast_avg

        } else if (fill_method == "source") {
          # load unbuffered values
          sel_rast_fil <- sel_rast_poly

        } else {
          stop("Value of attribute \"fill_method\" not recognised.")
        }

      }


      ## 4. Crop on borders

      if (fill_na==TRUE) {
        # if fill_na, mask only outside polygon
        writeRaster(sel_rast_fil, sel_rast_fil_path)
        gdal_warp(
          sel_rast_fil_path, sel_rast_crop_path,
          mask = if (fill_borders==FALSE & buffer<0) {sel_vect_buf_path} else {sel_vect_path}
        )
        sel_rast_crop <- cast_rast(sel_rast_crop_path, "rastobject")

      } else if (buffer<0) {
        # if buffer<0 but not filling NA, mask outside polygon AND inside holes
        sel_rast_crop <- sel_rast_fil
        if (fill_borders==TRUE) {
          sel_rast_crop[is.na(sel_rast_poly)] <- NA
        } else {
          sel_rast_crop[is.na(sel_rast_buf)] <- NA
        }
        writeRaster(sel_rast_crop, sel_rast_crop_path)

      } else {
        # if buffer>=0, no masking is needed (holes are left as original)
        sel_rast_crop <- sel_rast_poly
      }



      ## 4. Standardise
      sel_rast_z <- standardise(sel_rast_crop, sel_rast_avg, sel_rast_std)


      ## 5. Export output raster
      writeRaster(sel_rast_z, sel_rast_z_path)
      print(sel_rast_z_path)
      sel_rast_z_path

    } # end of continue_sel_poly IF cycle

  } # end of in_vect_buf FOREACH cycle

  ## Mosaic single rasters
  if (length(rast_z_paths)==1) {
    # FIXME this IF cycle provides the following error (?):
    #  Error in setValues(r, as.vector(t(x))) :
    #    values must be numeric, integer, logical or factor
    rast_z <- raster(rast_z_paths)
  } else {
    message(paste0(
      "[",Sys.time(),"] ",
      "Mosaicing single polygons into the final raster..."
    ))
    rast_z_list <- lapply(rast_z_paths,raster)
    rast_z_list$fun<- mean
    rast_z <- do.call(mosaic, rast_z_list)
  }

  # close connections (doing it after having mosaiced them,
  # because stopCluster deletes the temporary files of each R session)
  if (parallel) {
    parallel::stopCluster(cl = cluster)
  }

  # write output
  if (!is.na(out_file)) {
    if (overwrite!=TRUE & file.exists(out_file)) {
      warning(paste0(
        "Output file \"",out_file,"\" already exists; ",
        "output has been saved in a temporary file: \"",
        out_file <- file.path(tempdir(),basename(out_file)),"\"."
      ))
    }
    sgdf_z <- as(rast_z, "SpatialGridDataFrame")
    sgdf_z@data[,1][is.na(sgdf_z@data[,1])] <- NA # NaN to NA
    writeGDAL(
      sgdf_z, out_file,
      drivername = format,
      type = "Float32", mvFlag = -2^31, # not correct for Float32, but managed by QGIS
      options = if(format == "GTiff") {paste0("COMPRESS=",compress)}#,
      # overwrite = overwrite
    )
  } else {
    rast_z
  }

}


#' @export
#' @rdname standardise_rast
standardize_rast <- standardise_rast

#' @export
#' @rdname standardise_rast
zscore_rast <- function(in_rast,
                        out_file = NA,
                        in_vect = NULL,
                        by_poly  = TRUE,
                        min_area = 0,
                        fill_method = "focal",
                        fill_na = TRUE,
                        fill_borders = TRUE,
                        buffer = 0,
                        parallel = TRUE,
                        format = NA,
                        compress = "DEFLATE",
                        overwrite = FALSE
) {
  standardise_rast(in_rast,
                   out_file = out_file,
                   in_vect = in_vect,
                   by_poly  = by_poly,
                   min_area = min_area,
                   method = "zscore",
                   fill_method = fill_method,
                   fill_na = fill_na,
                   fill_borders = fill_borders,
                   buffer = buffer,
                   parallel = parallel,
                   format = format,
                   compress = compress,
                   overwrite = overwrite)
}

#' @export
#' @rdname standardise_rast
fixborders_rast <- function(in_rast,
                            out_file = NA,
                            in_vect = NULL,
                            min_area = 0,
                            fill_method = "focal",
                            fill_na = TRUE,
                            fill_borders = TRUE,
                            buffer = 0,
                            parallel = TRUE,
                            format = NA,
                            compress = "DEFLATE",
                            overwrite = FALSE
) {
  standardise_rast(in_rast,
                   out_file = out_file,
                   in_vect = in_vect,
                   by_poly  = TRUE,
                   min_area = min_area,
                   method = "input",
                   fill_method = fill_method,
                   fill_na = fill_na,
                   fill_borders = fill_borders,
                   buffer = -abs(buffer),
                   parallel = parallel,
                   format = format,
                   compress = compress,
                   overwrite = overwrite)
}

#' @export
#' @rdname standardise_rast
fillgaps_rast <- function(in_rast,
                          out_file = NA,
                          fill_method = "focal",
                          format = NA,
                          compress = "DEFLATE",
                          overwrite = FALSE
) {
  standardise_rast(in_rast,
                   out_file = out_file,
                   in_vect = NULL,
                   by_poly  = FALSE,
                   min_area = 0,
                   method = "input",
                   fill_method = fill_method,
                   fill_na = TRUE,
                   fill_borders = FALSE,
                   buffer = 0,
                   parallel = FALSE,
                   format = format,
                   compress = compress,
                   overwrite = overwrite)
}
