#' @title Retrieve statistics about value of a raster object
#' @description Function used quickly retrieve statistics (min, max, mean, median
#'   sd, and eventually quantiles) for bands of a raster object
#' @param in_rast a `Raster` object or the filename of a valid raster
#' @param quantiles `logical` If TRUE, retrieve also the quantiles, for each band,
#'   Default; FALSE
#' @param hist `logical` If TRUE, retrieve also the frequency histogram, for
#'   each band, Default: FALSE
#' @param verbose if FALSE suppress messages
#' @return `list` containing the following object:
#'   - nbands: `numeric` Number of bands of the object/file;
#'   - indbands: `numeric` If the object is associated with a file on disk, bands of the
#'     file to which each layer of the object correspond (see examples);
#'   - ncols: `numeric` Number of columns;
#'   - nrows: `numeric` Number of rows;
#'   - ncells: `numeric` Number of cells;
#'   - bnames: `character` Band names;
#'   - fnames: `character` If the object is associated with one or more files on disk,
#'     filename(s) corresponding to the different bands;
#'   - Z: `ANY` if the object has a "z" attribute set using raster::setZ(), values of the
#'     z attribute;
#'   - dtype: `character` datatype of the input raster (`raster` conventions)
#'   - proj4string: `character` proj4string of the object/file
#'   - units: `character` distance units of the projections
#' @examples
#' \dontrun{
#'  in_rast <- system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'   package = "sprawl.data")
#'  get_rastinfo(in_rast)
#'  in_rast <- system.file("extdata/OLI_test", "oli_multi_1000_b1.tif",
#'   package = "sprawl.data")
#'  get_rastinfo(in_rast)
#'  }
#' @rdname get_rastinfo
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom glue glue

get_raststats <- function(in_rast,
                          quantiles = FALSE,
                          hist      = FALSE) {

  in_rast <- cast_rast(in_rast, "rastobject")
  in_vrt  <- create_virtrast(in_rast, tempfile(fileext = ".vrt"))

  gdalinfo_str <- paste(
    in_vrt,
    "-stats",
    ifelse(hist, "-hist", "")
  )


  #   __________________________________________________________________________
  #   Launch gdalinfo over the vrt file                                   ####

  stats <- system2("gdalinfo" , args = gdalinfo_str, stdout = T)
  if (!is.null(attr(stats, "status"))) {
    stop("get_raststats --> Extraction of statistics failed. Aborting! \n\n",
         "The call was: \n",
         paste0("gdalinfo ", gdalinfo_str), "\n\n",
         "gdalinfo replied: \n",
         strwrap(stats[1]))
  }

  #   __________________________________________________________________________
  #   Extract basic stats to df_stats                                       ####

  df_stats <- stats[which(lapply(stats,
                                 FUN = function(x) grep("Minimum=", x)) == 1)]
  df_stats <- data.table::rbindlist(lapply(df_stats, FUN = function(x) {
    vals <- as.numeric(stringr::str_split_fixed(x, "," ,4) %>%
                         stringr::str_split_fixed("=" ,4) %>% .[,2])
    data.frame(min = vals[1], max = vals[2], avg = vals[3], sd = vals[4])
  }))
  df_stats[["band"]] <- seq_len(dim(df_stats)[1])

  #   __________________________________________________________________________
  #   If necessary, extract the histogram and compute quantiles             ####

  if (hist | quantiles) {
    pos_hist <- which(lapply(stats,
                             FUN = function(x) grep("buckets from", x)) == 1)

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
      bucks_vals <- bucks_minmax[1] + buck_size * seq(0,n_bucks - 1, 1)
      bucks_counts <- as.numeric(
        stringr::str_split_fixed(trimws(stats[[x + 1]]),
                                 "[ ]+",
                                 n_bucks)) %>%
        na.omit()
      bucks_freqs <-
      hist <- data.frame(value   = bucks_vals,
                         count   = bucks_counts,
                         freq    = bucks_counts / sum(bucks_counts),
                         cumfreq = cumsum(bucks_counts / sum(bucks_counts)))
      hist
    }

    hist_list <- lapply(pos_hist, FUN = function(x) get_hist(x))
    for (band in seq_len(length(hist))) {
      hist_list[[band]][["band"]] = band
    }
    hist_df <- data.table::rbindlist(hist)

    if (quantiles) {

      get_quantiles <- function(x) {

        cuts <- cut(x$cumfreq, seq(0,1,0.01), ordered_result = T)
        quants <- data.frame(quant = seq(0,1,0.01),
                             val   = quantile(x[["cumfreq"]], seq(0,1,0.01),
                           na.rm = TRUE))

      }

      # %>%
      #   dplyr::group_by(band) %>%
      #   # dplyr::mutate(quant = cut(cumfreq, quants)) %>%
      #   # dplyr::group_by(band, quant) %>%
        # dplyr::summarize(aaa = first(value))

    }


  }

}


# file <- "D:/Temp/buttami/OLI/LC08_L1TP_194028_20170422_20170501_01_T1_B4.TIF"
# rast <- raster::raster(file)
#
# sum <- summary(rast)
