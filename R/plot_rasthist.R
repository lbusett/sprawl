#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION, Default: 'freq'
#' @param type PARAM_DESCRIPTION, Default: 'hist'
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' #EXAMPLE1
#'  }
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
  if (verbose) {
    message("plot_rasthist --> Plotting histogram of `",
            call[[2]], "`")
  }


  #   ____________________________________________________________________________
  #   compute statistics if necessary                                         ####
  if (!inherits(in_rast, "Raster")) {
    hist_data <- get_raststats(in_rast, hist = TRUE)$hist
  } else {
    # TODO recheck this after implementing set_ratinfo !!!!!
    if (!assertthat::has_attr(in_rast, "rastinfo")) {
      hist_data <- get_raststats(in_rast, hist)
    } else {
      hist_data <- in_rast@rastinfo$stats$hists
    }
  }
  plot <- ggplot(hist_data) + theme_bw()
  if (type == "hist") {
    plot <- plot + ggplot2::geom_col(
      aes_string(x = "value", y = ifelse(variable == "count", "count", "freq")),
      width = (diff(hist_data$value))[1]) +
      facet_wrap(~band) +
      ggtitle("Frequency Histogram")
  } else {
    plot <- plot + ggplot2::geom_line(
      aes_string(x = "value", y = ifelse(variable == "count", "count", "freq"))) +
      facet_wrap(~band) +
      ggtitle("Frequency Histogram")
  }


}
