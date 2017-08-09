#' @title helper function used to extract statistics of raster values within
#'   `sprawl::extract_rast` and `sprawl::aggregate_rast`
#' @description FUNCTION_DESCRIPTION
#' @param in_data `data.table` containing raster values (for a full file or a "chunk")
#' @param var `character` name of the variable (i.e., column) to be used as grouping
#'   variable for summarization
#' @param comp_quant `logical` if TRUE, percentiles of the distribution are computed
#'   alongside standard statistics
#' @param FUN `function` if not NULL and is  a valid function, FUN is used for summarization
#'   and only its results are reported. Useful for example for analyses over large
#'   high-res raster since it reduces memory footprint (see `sprawl::aggregate_rast`)
#' @param band_n PARAM_DESCRIPTION
#' @param selband PARAM_DESCRIPTION
#' @return `list`

#' @importFrom stats median sd quantile
#'
summarize_data <- function(in_data,
                           var,
                           comp_quant,
                           FUN,
                           band_n,
                           selband) {

  #   ____________________________________________________________________________
  #   If a custom function was passed, compute only that                      ####

  # TODO better implementation of "custom" functions
  if (!is.null(FUN)) {
    if (!inherits(FUN, "function")) {
      stop("summarize_data --> FUN must be the name of a function to be applied to
summarize your data ! Aborting !")
    }

    myfundata  = in_data[, lapply(.SD, eval(FUN), na.rm = TRUE),
                         by = var,
                         .SDcols = c("value")]

    summ <- in_data[, list(band_n = band_n ,
                           date   = selband,
                           N_PIX  = .N),
                    by = var]
    summ[,"myfun" := myfundata$value]
  } else {

    #   ____________________________________________________________________________
    #   If a custom function was passed, compute only that                      ####

    summ <- in_data[, list(band_n = band_n ,
                           date   = selband,
                           N_PIX  = length(is.na(value) == TRUE),
                           avg    = mean(value, na.rm = T),
                           med    = as.double(stats::median(value, na.rm = T)),
                           sd     = stats::sd(value, na.rm = T),
                           min    = as.double(min(value, na.rm = T)),
                           max    = as.double(max(value, na.rm = T))
    ) , by = var]

    if (comp_quant) {
      quantiles <- in_data[, list(q01 = stats::quantile(value, .01, na.rm = TRUE),
                                  q05 = stats::quantile(value, .05, na.rm = TRUE),
                                  q15 = stats::quantile(value, .15, na.rm = TRUE),
                                  q25 = stats::quantile(value, .25, na.rm = TRUE),
                                  q35 = stats::quantile(value, .35, na.rm = TRUE),
                                  q45 = stats::quantile(value, .45, na.rm = TRUE),
                                  q55 = stats::quantile(value, .55, na.rm = TRUE),
                                  q65 = stats::quantile(value, .65, na.rm = TRUE),
                                  q75 = stats::quantile(value, .75, na.rm = TRUE),
                                  q85 = stats::quantile(value, .85, na.rm = TRUE),
                                  q95 = stats::quantile(value, .95, na.rm = TRUE),
                                  q99 = stats::quantile(value, .99, na.rm = TRUE))
                           , by = var]

      summ <- cbind(summ, quantiles[,2:13])
    }
  }

  return(summ)
}
