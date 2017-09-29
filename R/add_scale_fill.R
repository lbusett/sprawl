#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plot PARAM_DESCRIPTION
#' @param palette PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION
#' @param na.color PARAM_DESCRIPTION
#' @param zlims PARAM_DESCRIPTION
#' @param leg_breaks PARAM_DESCRIPTION
#' @param leg_labels PARAM_DESCRIPTION
#' @param leg_type PARAM_DESCRIPTION
#' @param outliers_style PARAM_DESCRIPTION
#' @param direction PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_scale_fill
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom scales squish censor
#' @importFrom ggplot2 aes_string scale_fill_brewer scale_fill_hue
#'   scale_fill_distiller

add_scale_fill <- function(plot,
                           palette,
                           title,
                           na.color,
                           zlims,
                           leg_breaks,
                           leg_labels,
                           leg_type,
                           outliers_style,
                           direction,
                           ...) {

  # ____________________________________________________________________________
  # Qualitative palette: use scal_fill_hue or scale_fill_brewer             ####
  # TODO add support for manual categorical fill scale
  if (palette$category_2 == "qual") {
    if (palette$source == "brewer") {
      plot <- plot + scale_fill_brewer(type = "qual",
                                       palette = as.character(palette$name),
                                       na.value = ifelse(is.null(na.color),
                                                         "grey50", na.color)
      )
    } else {
      plot <- plot + scale_fill_hue(na.value = ifelse(is.null(na.color),
                                                      "grey50", na.color))
    }
  } else {
    # ____________________________________________________________________________
    # Continuous palette: use scal_fill_brewer                                ####
    # TODO add support for scale_fill_gradient and scale_fill_viridis
    if (palette$source == "brewer") {
      plot <- plot + scale_fill_distiller(
        title,
        limits  = zlims,
        breaks  = if (is.null(leg_breaks)) waiver() else leg_breaks,
        labels  = if (is.null(leg_labels)) waiver() else leg_labels,
        type    = palette$category,
        guide   = leg_type,
        palette = as.character(palette$name),
        oob     = ifelse(outliers_style == "to_minmax",
                         scales::squish, scales::censor), #nolint
        direction = direction,
        na.value = ifelse(is.null(na.color), "grey50", na.color),
        trans    = ifelse(!exists("trans"), "identity", trans)
      )
    }
  }
  plot
}
