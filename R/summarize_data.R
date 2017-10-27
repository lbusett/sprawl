#' @title helper function used to extract statistics of raster values within
#'   `sprawl::extract_rast`
#' @description helper function for `sprawl::extract_rast` used to extract
#'   statistics of values of `in_rast` for each polygon of the vector object
#'  passed as input to `extract_rast`
#' @param in_data `data.table` containing raster values (for a full file or a
#'  "chunk")
#' @param rast_type `character` ("continuous" | "categorical) specifies if the
#'   values passed in `in_data` represent a continuous or categorical variable.
#'  (see @description)
#' @param group_var `character` name of the group_variable (i.e., column) to be
#'   used as grouping group_variable for summarization. Always equal to "mdxtnq"
#'   for now !
#' @param comp_quant `logical` if TRUE, percentiles of the distribution are
#'   computed alongside standard statistics, Default: FALSE (Ignored if
#'   rast_type == "categorical)
#' @param comp_freq `logical` if TRUE, frequencies of the the different classes
#'   of a categorical raster present in each polygon are also returned,
#'   Default = FALSE (Ignored if rast_type == "continuous")
#' @param FUN `function` if not NULL and is  a valid function, FUN is used for
#'   summarization and only its results are reported. Useful for example for
#'   analyses over large high-res raster since it reduces memory footprint
#'   (see `sprawl::aggregate_rast`)
#' @param na.rm `logical` If TRUE, nodata values are not considered in statistics
#'   calculations for continuous variables, Default = FALSE
#' @param band_n `numeric` band of the original raster from which values passed
#'   in `in_data` are taken.
#' @param selband `character` or `Date` bandname or acquisition data of the
#'   band of the original raster from which values passed in `in_data` are taken
#' @return `data.table` containing different data as a function of `rast_type`.
#'   (See [`extract_rast`])
#' @export
#' @importFrom stats median sd
#' @importFrom data.table dcast setnames

summarize_data <- function(in_data,
                           rast_type,
                           group_var,
                           comp_quant,
                           comp_freq,
                           FUN,
                           na.rm = TRUE,
                           band_n,
                           selband) {

  .N <- N <- perc <- . <- .SD <- value <- NULL


  #   __________________________________________________________________________
  #   CASE 1: Continuous variable                                           ####
  #   In this case, the output contains results of application of FUN on groups
  #   of in_data OR standard statitics of groups (avg, min, max, sd, quantile (
  #   if comp_quant = TRUE))

  if (rast_type == "continuous") {
    #   __________________________________________________________________________
    #   If a custom function was passed, compute only that                    ####

    # TODO better implementation of "custom" functions - maybe allowing use of
    # a list of functions !
    if (!is.null(FUN)) {
      if (!inherits(FUN, "function")) {
        stop("summarize_data --> FUN must be the name of a function to be " ,
             "applied to summarize your data ! Aborting !")
      }

      myfundata <- in_data[, lapply(.SD, eval(FUN), na.rm = TRUE),
                           by = group_var,
                           .SDcols = c("value")]

      summ <- in_data[, list(band_n = band_n ,
                             date   = selband,
                             n_pix  = .N,
                             n_pix_val = .N),
                      by = group_var]
      summ[,"myfun" := myfundata$value]

    } else {

      #   ______________________________________________________________________
      #   Normal use: compute avf, sd, etcetera                             ####

      summ <- in_data[, list(band_n    = band_n ,
                             date      = selband,
                             n_pix     = length(value),
                             n_pix_val = length(which(is.na(value) == FALSE)),
                             avg       = mean(value, na.rm = na.rm),
                             med       = as.double(stats::median(value, na.rm = na.rm)), #nolint
                             sd        = stats::sd(value, na.rm = na.rm),
                             min       = as.double(min(value, na.rm = na.rm)),
                             max       = as.double(max(value, na.rm = na.rm))
      ) , by = group_var]

      if (comp_quant) {

        #   ____________________________________________________________________
        #   if comp_quant passed, also compute percentiles                  ####
        quantiles <- in_data[, list(
          q01 = stats::quantile(value, .01, na.rm = TRUE),
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
          , by = group_var]
        summ <- cbind(summ, quantiles[,2:13])
      }
    }

  } else {

    #   ________________________________________________________________________
    #   CASE 2: Categorical variable                                        ####
    #   In this case, the output contains the value corresponding to the
    #   majority category found in each group (a.k.a. polygon), plus the
    #   percentage of pixels belonging to each category found in each polygon.
    #

    #functionj to get the MODE for a factor
    #(https://stackoverflow.com/questions/32283880/r-apply-mean-for-numerical-columns-and-majority-vote-on-categorical) #nolint

    mode <- function(x, na.rm) {
      if (na.rm) x <- na.omit(x)
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    summ <- in_data[, list(band_n    = band_n,
                           date      = selband,
                           n_pix     = length(value),
                           n_pix_val = length(which(is.na(value) == FALSE)),
                           mode      = mode(value, na.rm = na.rm)),
                    by = c("mdxtnq")]

    if (comp_freq) {
      #   ____________________________________________________________________
      #   if comp_freq passed, also compute number of pixels per class    ####

      freqs <- in_data[, .N, by = c("value", "mdxtnq")] %>%
        .[, perc := N/sum(N), by = c("mdxtnq")] %>%
        data.table::dcast(mdxtnq ~ value, value.var = "perc") %>%
        data.table::setnames(., c("mdxtnq", paste0("freq_", names(.)[2:length(.)])))
      freqs[is.na(freqs)] <- 0
      summ <- merge(summ, freqs)

    }
  }
  return(summ)
}
