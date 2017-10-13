#' @title create list of valid fill palettes
#' @description helper function used to create a `data.frame` containing info
#'   on valid fill palettes for `plot_rast`, `plot_rast_gg` and `plot_vect`
#' @return a `data.frame` containing info on valid fill palettes that can used
#'  in `plot_rast`, `plot_rast_gg` and `plot_vect`
#' @details DETAILS
#' @examples
#' \dontrun{
#' # Print the list of valid fill palettes used in `plot_rast`, `plot_rast_gg`
#' # and `plot_vect`
#' fillpals()
#' }
#' @rdname fillpals
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom dplyr select arrange
#' @importFrom RColorBrewer brewer.pal.info
fillpals <- function() {

  name <- source <- category <- cont_qual <- maxcolors <- colorblind <- NULL
  # -----------------------------------------------------------
  # Create list of valid palette names and chars

  fill_pals   <- rbind(
    cbind(name = row.names(RColorBrewer::brewer.pal.info),
          RColorBrewer::brewer.pal.info, source = "brewer"),
    data.frame(name = "hue", maxcolors = 1000, category = "qual",
               colorblind = FALSE, source = "ggplot"),
    make.row.names = FALSE
  )

  fill_pals$category  <- as.character(fill_pals$category)
  fill_pals$cont_qual <- "qual"
  fill_pals$cont_qual[fill_pals$category %in% c("div", "seq")] <- "cont"
  fill_pals <- dplyr::select(fill_pals,
                             name, source, category, cont_qual, maxcolors,
                             colorblind) %>%
    dplyr::arrange(cont_qual, category)

  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####

  return(fill_pals)

}
