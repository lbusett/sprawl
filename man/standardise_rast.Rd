% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardise_rast.R
\name{standardise_rast}
\alias{standardise_rast}
\alias{standardize_rast}
\alias{zscore_rast}
\alias{rbias_rast}
\alias{fixborders_rast}
\alias{fillgaps_rast}
\title{Generate a standardised raster}
\usage{
standardise_rast(in_rast, out_file = NA, in_vect = NULL,
  by_poly = TRUE, min_area = 0, method = "zscore",
  fill_method = "focal", fill_na = TRUE, fill_borders = TRUE,
  buffer = 0, parallel = TRUE, format = NA, dataType = NA,
  scaleFactor = NA, compress = "DEFLATE", overwrite = FALSE)

standardize_rast(in_rast, out_file = NA, in_vect = NULL,
  by_poly = TRUE, min_area = 0, method = "zscore",
  fill_method = "focal", fill_na = TRUE, fill_borders = TRUE,
  buffer = 0, parallel = TRUE, format = NA, dataType = NA,
  scaleFactor = NA, compress = "DEFLATE", overwrite = FALSE)

zscore_rast(in_rast, out_file = NA, in_vect = NULL, by_poly = TRUE,
  min_area = 0, fill_method = "source", fill_na = TRUE,
  fill_borders = TRUE, buffer = 0, parallel = TRUE, format = NA,
  dataType = NA, scaleFactor = NA, compress = "DEFLATE",
  overwrite = FALSE)

rbias_rast(in_rast, out_file = NA, in_vect = NULL, by_poly = TRUE,
  min_area = 0, fill_method = "source", fill_na = TRUE,
  fill_borders = TRUE, buffer = 0, parallel = TRUE, format = NA,
  dataType = NA, scaleFactor = NA, compress = "DEFLATE",
  overwrite = FALSE)

fixborders_rast(in_rast, out_file = NA, in_vect = NULL, min_area = 0,
  fill_method = "focal", fill_na = TRUE, fill_borders = TRUE,
  buffer = 0, parallel = TRUE, format = NA, dataType = NA,
  scaleFactor = 1, compress = "DEFLATE", overwrite = FALSE)

fillgaps_rast(in_rast, out_file = NA, fill_method = "focal",
  format = NA, compress = "DEFLATE", overwrite = FALSE)
}
\arguments{
\item{in_rast}{The input raster or path.}

\item{out_file}{The path of the output raster; if NULL (default),
a RasterLayer is returned by the function).}

\item{in_vect}{The vector file or object containing the polygons used as
mask and as polygons within which computing the standardised values.}

\item{by_poly}{Logical: if TRUE (default), input raster values are
standardised basing on the averages and standard deviations computed
within each polygon; if FALSE, they are computed on the whole
polygons.}

\item{min_area}{Numeric: the minimum area (in squared metres) that polygons
must have to be considered and included in the final raster; default is
0 (all non empty polygons are considered). Notice that, if a value of
\code{buffer} > 0 is set, the value of \code{min_area} applies also to buffered
polygons.}

\item{method}{Character: the method used to standardise values.
Accepted values are:
\itemize{
\item \code{"zscore"} (default): compute the Z-score (real standardisation);
\item \code{"rbias"}: compute the rbias;
\item \code{"center"}: center values over the average;
\item \code{"input"}: do not standardise values (this method makes sense only
with \code{fill_method != "input"}, as a method to
interpolate values of border or holes).
}}

\item{fill_method}{Character: the method used to fill buffered areas
(if \code{buffer < 0}) and holes (if \code{fill_na = TRUE}).
Accepted values are:
\itemize{
\item \code{"source"}: (default, faster method): standardised input values
are maintained (but they do not concur to compute average and
standard deviation values used to standardise);
\item \code{"focal"} (slower): a focal weight is applied to borders, so values
near the border of buffers are expanded (see details in the function
code);
\item \code{"average"}: the averaged value within each polygon buffer
(corresponding to 0 with \code{method = "zscore"}, \code{"rbias"} or \code{"center"})
is repeaded;
}}

\item{fill_na}{Logical: if TRUE (default), NA values within polygons are
filled using the method selected with argument \code{fill_method};
if FALSE, they are letf to NA.}

\item{fill_borders}{Logical: if TRUE (default), values between the border of
the polygons and the negative buffers around the polygons are filled using
the method set with argument \code{fill_method}; if FALSE, they are left to NA.
This argument makes sense only with \code{buffer < 0}.}

\item{buffer}{Numeric: the buffer (in metres) applied to polygons usd to
compute average and standard deviation (default is 0).
If \code{fill_borders = TRUE} (default) pixels which are outside this buffer are
still computed in the final raster (with the method chosen with
\code{fill_method} argument), but their input values do not concur to compute
average and standard deviation. Notice that, in order to exclude values
of borders, a negative value of \code{buffer} must be provided.}

\item{parallel}{Logical or integer: if TRUE, the algorythm used to compute
output values is runned parallelised on single polygons and the number of
cores is automatically determined (otherwise, an integer value can be
provided in order to set this value manually); if FALSE (or 1), it is
runned on a single core. This value is not considered if \code{by_poly = TRUE}
(in this case, it is runned on a single core).}

\item{format}{(optional) Format of the output file (in a
format recognised by GDAL). Default is to maintain each input format.}

\item{dataType}{(optional) Numeric datatype of the ouptut rasters.
if "Float32" or "Float64" is chosen, numeric values are not rescaled;
if "Int16" or "UInt16", values are multiplicated by \code{scaleFactor} argument.
If "NA" is used (default), the same dataType of \code{in_rast} is used.}

\item{scaleFactor}{(optional) Scale factor for output values when an integer
datatype is chosen (default values with \code{method = "zscore"} are 1000
for "Int16" and "UInt16" and 1E8 for "Int32" and "UInt32";
with different \code{method} values, defalt is 1).
Notice that, using "UInt16" and "UInt32" types,
negative values will be truncated to 0.}

\item{compress}{(optional) In the case a GTiff format is
present, the compression indicated with this parameter is used.
Default is "DEFLATE".}

\item{overwrite}{Logical value: should existing output files be
overwritten? (default: FALSE)}
}
\value{
NULL if \code{out_file} is not NULL; the output RasterLayer otherwise.
}
\description{
This function can be used to process a raster in order to
\enumerate{
\item obtain standardised values within polygons of a specified
vector file or R spatial object, and/or
\item replace values of the raster near to the borders of the polygons with
local or global averages, and/or
\item fill holes (NA values) with local or global averages.
}

Some aliases are set for specific functions (see details).
}
\details{
Some aliases are set for specific functions:
\itemize{
\item \code{zscore_rast()} computes the Z-score on the input raster
(equivalent to \code{standardise_rast(method = "zscore")});
\item \code{rbias_rast()} computes the rbias on the input raster
(equivalent to \code{standardise_rast(method = "rbias")});
\item \code{fixborders_rast()} can be used to replace border values
(equivalent to \code{standardise_rast(method = "input", by_poly = TRUE, dataType = NA, scaleFactor = 1)});
\item \code{fillgaps_rast()} is used to fill gaps (NA values) without using
polygon masks
(equivalent to \code{standardise_rast(by_poly = FALSE, min_area = 0, method = "input", fill_na = TRUE, fill_borders = FALSE, buffer = 0, parallel = FALSE, dataType = NA, scaleFactor = 1)}).
}

By default, input raster values are standardised basing on the averages
and standard deviations computed within each polygon (it is possible to
specify a buffer value for computing them setting the \code{buffer} argument,
i.e. in order to exclude border values).
Oterwhise, setting \code{by_poly = FALSE} these metrics are computed on the whole
polygons (so raster values are simply linearly combined and masked on the
polygon surface).
}
\examples{
\dontrun{
# These examples cannot be runned globally;
# a global example will be done in future.

# Define input files and paths
projects_dir  <- "/mnt/projects" # 10.0.1.230
nrworking_dir <- "/mnt/nr_working" # 10.0.1.230
data_dir <- file.path(nrworking_dir,"luigi/data/s2tsp/180117_test_zscore/MSAVI")
raster_ex <- raster(file.path(data_dir,"S2B2A_20180104_022__MSAVI_10.tif"))
source(file.path(nrworking_dir,"luigi/code/satfarming/opencon_satfarming.R"))
appez <- st_read_db(
  con_satfarming,
  query = "SELECT * FROM bf_jolanda.appez WHERE anno = 2017;"
)

# Example 1: negative big buffer, focal method, fill NA
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore1.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -30, min_area=1E4,
  parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "focal", fill_na = TRUE
)

# Example 1.1: negative big buffer, focal method, do not fill NA
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore1_1.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -30, min_area=1E4,
  parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "focal", fill_na = FALSE
)

# Example 2: negative buffer, average method
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -10, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "average"
)

# Example 2.1: negative buffer, average method, fill_na=FALSE
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_1.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -10, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "average", fill_na = FALSE
)

# Example 2.2: negative buffer, average method, do not fill borders
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_2.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -20, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "average", fill_borders = FALSE
)

# Example 2.3: negative buffer, do not fill neither borders nor holes
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore2_3.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = -20, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "indifferent", fill_borders = FALSE, fill_na = FALSE
)

# Example 3: no buffer, only fill holes
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore3.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = 0, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "focal"
)

# Example 4: do not standardise nor buffer, only fill holes (average)
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore4.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = 0, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "input", fill_method = "average"
)

# Example 5: positive (exagerated) buffer
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore5.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=TRUE, buffer = 250, min_area=1E4, parallel=TRUE, overwrite=TRUE,
  method = "zscore", fill_method = "focal"
)

# Example 6: remove borders and holes, globally
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore6.tif"))
tmp <- standardise_rast(
  raster_ex, out_file, in_vect=appez[58:60,],
  by_poly=FALSE, buffer = -30, min_area=1E4, overwrite=TRUE,
  method = "zscore", fill_method = "focal"
)

# Example 7: fill holes without masking over polygons
out_file <- file.path(data_dir,paste0(raster_ex@data@names,"_zscore7.tif"))
tmp <- standardise_rast(
  raster_ex, out_file,
  buffer = "indifferent", min_area=1E4, overwrite=TRUE,
  method = "input", fill_method = "focal"
)
}

}
\author{
Luigi Ranghetti, phD (2018) \href{mailto:ranghetti.l@irea.cnr.it}{ranghetti.l@irea.cnr.it}

Lorenzo Busetto, phD (2017) \href{mailto:lbusett@gmail.com}{lbusett@gmail.com}
}
