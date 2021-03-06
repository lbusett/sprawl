% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cuttails_rast.R
\name{cuttails_rast}
\alias{cuttails_rast}
\title{remove outliers from a raster image}
\usage{
cuttails_rast(in_rast, tails = c(0.02, 0.98), to_file = FALSE,
  verbose = TRUE)
}
\arguments{
\item{in_rast}{\code{*raster} object (single layer or stack), or filename of a raster file}

\item{tails}{\code{numeric array (2)} quantiles of the distribution of pixel values used for the cut.
For example, setting tails = c(0.05, 0.95) will set to NoData all pixels below the 5th and
above the 95th percentile,  Default: c(0.02, 0.98)}

\item{to_file}{DESCRIPTION}

\item{verbose}{DESCRIPTION}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
the function allows to automatically set to NoData eventual outliers present in a
raster single- or multi-band object (or file). For each band, quantiles of the distribution
of pixel values are computed using \code{raster::quantile}, and the values falling outside the
limits set with the \code{tails} argument are set to NoData in the output
}
\details{
DETAILS
}
\examples{

 # on single band
 in_rast <- raster::raster(ncol=100, nrow=100)
 in_rast <- raster::setValues(in_rast, 1:10000)
 in_rast_cut <- cuttails_rast(in_rast)
 in_rast_cut

 # on multi band
 in_brick <- raster::stack(in_rast,in_rast,in_rast)
 in_brick
 in_brick_cut <- cuttails_rast(in_brick)
 in_brick_cut

}
