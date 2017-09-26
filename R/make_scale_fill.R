#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param palette PARAM_DESCRIPTION
#' @param na.color PARAM_DESCRIPTION
#' @param zlims PARAM_DESCRIPTION
#' @param leg_breaks PARAM_DESCRIPTION
#' @param leg_labels PARAM_DESCRIPTION
#' @param leg_type PARAM_DESCRIPTION
#' @param outliers_style PARAM_DESCRIPTION
#' @param direction PARAM_DESCRIPTION
#' @param ...	Other arguments passed on to [`ggplot2::discrete_scale()`] or
#'   [`ggplot2::continuous_scale] to control name, scale transformation ecc.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' #EXAMPLE1
#'  }
#' @rdname make_scale_fill
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom scales squish censor
#' @importFrom grid unit
make_scale_fill <- function(palette,
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
    if (palette$source == "brew") {
      fill_pal <- RColorBrewer::scale_fill_brewer(...,
        type = "qual",
        palette = palette_name,
        na.value = ifelse(is.null(na.color), "grey50", na.color)
      )
    } else {
      fill_pal <- scale_fill_hue(na.value = ifelse(is.null(na.color),
                                                   "grey50", na.color))
    }
  } else {
    # ____________________________________________________________________________
    # Continuous palette: use scal_fill_brewer                                ####
    # TODO add support for scale_fill_gradient and scale_fill_viridis
    if (palette$source == "brew") {
      fill_pal <- scale_fill_distiller(...,
        "Value",
        limits  = zlims,
        breaks  = ifelse(is.null(leg_breaks), waiver(), leg_breaks),
        labels  = ifelse(is.null(leg_labels), waiver(), leg_labels),
        type    = palette$category,
        guide   = leg_type,
        palette = palette,
        oob     = ifelse(outliers_style == "to_minmax", scales::squish, scales::censor), #nolint
        direction = direction,
        na.value = ifelse(is.null(na.color), "grey50", na.color))
    }
    fill_pal <- fill_pal + theme(legend.justification = "center",
                                 legend.box.spacing = grid::unit(0.5,"points"))
  }
}
