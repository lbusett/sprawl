#' @title Retrieve statistics about value of a raster object
#' @description Function used quickly retrieve statistics (min, max, mean, median
#'   sd, and eventually quantiles) for bands of a raster object
#' @param in_rast a `Raster` object or the filename of a valid raster
#' @param quantiles `logical` If TRUE, retrieve also the quantiles, for each band,
#'   Default; FALSE
#' @param hist `logical` If TRUE, retrieve also the frequency histogram, for
#'   each band, Default: FALSE
#' @param verbose if FALSE suppress messages, Default: TRUE
#' @return a `list` containing the following elements:
#'   - **stats**: `data.frame` containing min, max, average and stendard deviation
#'     for each band (see examples);
#'   - **quants**: `data.frame` containing the quantiles of the distribution
#'     of raster values, for each band (100 value - 0.01 interval) (NULL is returned
#'     if `quants` == FALSE (the default));
#'   - **hists**: `data.frame` containing information about the distribtion of
#'     raster values for each band. the data frame includes the limits of each
#'     bin, the count of the number of pixels included in it, the corresponding
#'     frequency and the cumulated frequency at each bin  (NULL is returned
#'     if `hists` == FALSE (the default));
#' @examples
#' \dontrun{
#'  in_rast <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'   package = "sprawl.data")
#'  get_raststats(in_rast)
#'
#'  get_raststats(in_rast, quantiles = T)
#'
#'  get_raststats(in_rast, hist = T, quantiles = T)
#'  }
#' @rdname get_raststats
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom data.table rbindlist
#' @importFrom stringr str_split_fixed
#' @importFrom magrittr "%>%"

get_raststats <- function(in_rast,
                          quantiles = FALSE,
                          hist      = FALSE,
                          verbose   = TRUE)  {

  . <- NULL
  call <- match.call()

  if (verbose) {
    message("get_raststats --> Computing statistics of `" ,
            call[[2]], "`")
  }
  #   ____________________________________________________________________________
  #   prepare data and check args                                             ####

  rast_type <- get_rastype(in_rast)
  if (rast_type == "rastfile"){
    in_rast <- read_rast(in_rast)
  }
  in_vrt  <- create_virtrast(in_rast, tempfile(fileext = ".vrt"),
                             verbose = FALSE)

  #   __________________________________________________________________________
  #   Launch gdalinfo over the vrt file                                   ####
  if (hist | quantiles) get_hists = TRUE else get_hists = FALSE
  gdalinfo_str <- paste(
    in_vrt,
    "-stats",
    ifelse(get_hists, "-hist", "")
  )

  stats <- system2("gdalinfo" , args = gdalinfo_str, stdout = T)
  if (!is.null(attr(stats, "status"))) {
    stop("get_raststats --> Extraction of statistics failed. Aborting! \n\n",
         "The call was: \n",
         paste0("gdalinfo ", gdalinfo_str), "\n\n",
         "gdalinfo replied: \n",
         strwrap(stats[1]))
  }

  #   __________________________________________________________________________
  #   Extract basic stats to stats_df                                       ####

  stats_df <- stats[which(lapply(stats,
                                 FUN = function(x) grep("Minimum=", x)) == 1)]

  stats_df <- data.table::rbindlist(lapply(stats_df, FUN = function(x) {
    vals <- as.numeric(stringr::str_split_fixed(x, "," ,4) %>%
                         stringr::str_split_fixed("=" , n = 4) %>% .[,2])
    data.frame(min = vals[1], max = vals[2], avg = vals[3], sd = vals[4])
  }))

  stats_df[["band"]] <- seq_len(dim(stats_df)[1])

  #   __________________________________________________________________________
  #   If necessary, extract the histogram and compute quantiles             ####

  if (hist | quantiles) {
    pos_hist <- which(lapply(stats,
                             FUN = function(x) grep("buckets from", x)) == 1)

    # helper function to retrieve the counts from gdalinfo output ----
    get_hist <- function(x) {
      n_bucks <- as.numeric(stringr::str_split_fixed(stats[[x]],
                                                     "buckets from ",2)[1])
      bucks_minmax <- stringr::str_split_fixed(
        stats[[x]], "buckets from ",2)[2] %>%
        stringr::str_split_fixed(., " to ",
                                 n = 2) %>%
        gsub(":", "", .) %>%
        as.numeric()

      buck_size  <- diff(bucks_minmax)/n_bucks
      bucks_vals <- bucks_minmax[1] + buck_size * seq(0, n_bucks - 1, 1)
      bucks_counts <- as.numeric(
        stringr::str_split_fixed(trimws(stats[[x + 1]]),
                                 "[ ]+",
                                 n_bucks)) %>%
        na.omit()

      hist <- data.frame(value   = bucks_vals,
                         count   = bucks_counts,
                         freq    = bucks_counts / sum(bucks_counts),
                         cumfreq = cumsum(bucks_counts / sum(bucks_counts)))
      hist
    }

    hist_list <- lapply(pos_hist, FUN = function(x) get_hist(x))
    for (band in seq_len(length(hist_list))) {
      hist_list[[band]][["band"]] = band
    }
    hists_df <- data.table::rbindlist(hist_list)

    if (quantiles) {
      # helper function to estimate quantiles form histograms ----
      get_quantiles <- function(x) {

        q_lims <- seq(0,1,0.01)
        q_pos  <- list()
        for (q in seq_along(q_lims)) {
          q_pos[[q]] <- min(which(x$cumfreq >= q_lims[q]))
        }
        q_pos  <- unlist(q_pos)
        return(data.frame(quant = q_lims, val = x$value[q_pos]))

      }

      quants_list <- lapply(hist_list, FUN = function(x) get_quantiles(x))
      for (band in seq_len(length(quants_list))) {
        quants_list[[band]][["band"]] = band
      }
      quants_df <- data.table::rbindlist(quants_list)

    }

  }

  out <- list()
  out[["stats"]] <- stats_df
  if(quantiles) out[["quants"]] <- quants_df else out[["quants"]] <- NULL
  if(hist) out[["hists"]]       <- hists_df else out[["hists"]] <- NULL
  out
}

