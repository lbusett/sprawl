#' @title plot the frequency histogram of values of a raster
#' @description plot the frequency histogram of values of a raster, divided
#'   by band
#' @param in_rast a `Raster` object, or a file path corresponding to a valid
#'   raster file
#' @param variable `character ["count" | "freq"]`  varaible to be plotted,
#'   can be the countof pixels or their frequency, Default: 'freq'
#' @param type `character ["hist" or "line"]`, plotting style. Usea "bar" or a
#'   "line" plot, Default: 'line'
#' @param verbose If FALSE, suppress processing messages, Default: TRUE
#' @return A ggplot
#' @examples
#'   in_rast <- read_rast(system.file("extdata/OLI_test",
#'       "oli_multi_1000.tif", package = "sprawl.data"))
#'   plot_rasthist(in_rast, type = "line")
#' @rdname plot_rasthist
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat has_attr
#' @importFrom ggplot2 geom_col geom_line aes_string ggplot
#'   aes_string
plot_rasthist <- function(in_rast,
                          variable = "freq",
                          type     = "hist",
                          verbose  = TRUE) {
  call <- match.call()
  in_rast <- cast_rast(in_rast, "rastobject")
  if (verbose) {
    message("plot_rasthist --> Plotting histogram of `",
            call[[2]], "`")
  }

  info <- get_rastinfo(in_rast, stats = FALSE)
  #   ____________________________________________________________________________
  #   compute statistics if necessary                                         ####
  if (!inherits(in_rast, "Raster")) {
    hist_data <- get_raststats(in_rast, hist = TRUE)$hist
  } else {
    # TODO recheck this after implementing set_ratinfo !!!!!
    if (!assertthat::has_attr(in_rast, "rastinfo")) {
      hist_data <- get_raststats(in_rast, hist = TRUE)$hist
    } else {
      hist_data <- in_rast@rastinfo$stats$hists
    }
  }
  hist_data$band <- factor(hist_data$band, labels = info$bnames)
  plot <- ggplot(hist_data) + theme_bw()
  if (type == "hist") {
    plot <- plot + ggplot2::geom_col(
      aes_string(x = "value", y = ifelse(variable == "count", "count", "freq")),
      width = (diff(hist_data$value))[1]) +
      facet_wrap(~band)  +
      ggtitle("Frequency Histogram")
  } else {
    plot <- plot + ggplot2::geom_line(
      aes_string(x = "value", y = ifelse(variable == "count", "count", "freq"))) +
      facet_wrap(~band) +
      ggtitle("Frequency Histogram")
  }

  return(plot)
}
