% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_data.R
\name{summarize_data}
\alias{summarize_data}
\title{helper function used to extract statistics of raster values within
\code{sprawl::extract_rast}}
\usage{
summarize_data(in_data, rast_type, group_var, comp_quant, comp_freq, FUN,
  na.rm = TRUE, band_n, selband)
}
\arguments{
\item{in_data}{\code{data.table} containing raster values (for a full file or a
"chunk")}

\item{rast_type}{\code{character} ("continuous" | "categorical) specifies if the
values passed in \code{in_data} represent a continuous or categorical variable.
(see @description)}

\item{group_var}{\code{character} name of the group_variable (i.e., column) to be
used as grouping group_variable for summarization. Always equal to "mdxtnq"
for now !}

\item{comp_quant}{\code{logical} if TRUE, percentiles of the distribution are
computed alongside standard statistics, Default: FALSE (Ignored if
rast_type == "categorical)}

\item{comp_freq}{\code{logical} if TRUE, frequencies of the the different classes
of a categorical raster present in each polygon are also returned,
Default = FALSE (Ignored if rast_type == "continuous")}

\item{FUN}{\code{function} if not NULL and is  a valid function, FUN is used for
summarization and only its results are reported. Useful for example for
analyses over large high-res raster since it reduces memory footprint
(see \code{sprawl::aggregate_rast})}

\item{na.rm}{\code{logical} If TRUE, nodata values are not considered in statistics
calculations for continuous variables, Default = FALSE}

\item{band_n}{\code{numeric} band of the original raster from which values passed
in \code{in_data} are taken.}

\item{selband}{\code{character} or \code{Date} bandname or acquisition data of the
band of the original raster from which values passed in \code{in_data} are taken}
}
\value{
\code{data.table} containing different data as a function of \code{rast_type}.
(See \code{\link{extract_rast}})
}
\description{
helper function for \code{sprawl::extract_rast} used to extract
statistics of values of \code{in_rast} for each polygon of the vector object
passed as input to \code{extract_rast}
}
