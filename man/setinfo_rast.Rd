% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setinfo_rast.R
\name{setinfo_rast}
\alias{setinfo_rast}
\title{add \code{info} attribute to a raster object}
\usage{
setinfo_rast(object)
}
\arguments{
\item{object}{object of class \code{Raster}}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
add an \code{info} attribute containing info retrieved by
\code{sprawl::get_rastinfo} to a \code{Raster} object
}
\details{
DETAILS
}
\examples{

 setinfo_obj <- raster::brick(system.file("extdata/OLI_test", "oli_multi_1000.tif",
                                          package = "sprawl.data"))
 setinfo_obj <- setinfo_rast(setinfo_obj)
 setinfo_obj@info

}
\author{
Lorenzo Busetto, phD (2017) \href{mailto:lbusett@gmail.com}{lbusett@gmail.com}
}
