#' @title add a scale_fill palette to a ggplot
#' @description helper function to `plot_rast_gg` and `plot_vect` which adds
#'  and customizes the scale_fill palette based on characteristics of the plot
#'  (e.g., categorical or continuous variable), selected palette and options
#'  concerning treatment of outliers.
#' @param plot `gg` object to which the fill scale has to be added.
#' @param palette `data.frame (1)` Line of the d.f. created by `fillpals`
#'  corresponding to the desired palette.
#' @param title `character` Name to be used as title in the palette legend.
#' @inheritParams plot_vect
#' @param ... any other parameter to be passed to `ggplot::scale_fill_brewer` or
#'  `ggplot::scale_fill_hue`
#' @return The function is called for its side effects.
#' @rdname add_scale_fill
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom scales squish censor
#' @importFrom ggplot2 aes_string scale_fill_brewer scale_fill_hue
#'   scale_fill_distiller scale_fill_manual


add_scale_fill <- function(plot,
                           palette,
                           title,
                           na.color,
                           zlims,
                           leg_breaks,
                           leg_labels,
                           leg_colors,
                           leg_type,
                           outliers_style,
                           direction,
                           ...) {

  if (!exists("trans")) trans <- NULL
  # ____________________________________________________________________________
  # Qualitative palette: use scal_fill_hue or scale_fill_brewer             ####
  # or manual (requires also specifying leg_colors)

  if (palette[["cont_qual"]] == "qual") {

    if (palette[["name"]] == "manual") {

      if (is.null(leg_colors)) {
        stop("To use palette_name = \"manual\" you need also to use ",
             "`leg_colors` to specify the\n colors to be assigned to each value ",
             "of `fill_var`. Aborting!")
      }
      # check if the passed colours have the right length

      plot <- plot + scale_fill_manual(
        values   = leg_colors,
        labels   = if (is.null(leg_labels)) waiver() else leg_labels,
        na.value = ifelse(is.null(na.color), "grey50", na.color)
      )
    } else {
      if (palette$source == "brewer") {
        plot <- plot + scale_fill_brewer(
          type = "qual",
          palette = as.character(palette$name),
          labels   = if (is.null(leg_labels)) waiver() else leg_labels,
          na.value = ifelse(is.null(na.color), "grey50", na.color)
        )
      } else {
        plot <- plot + scale_fill_hue(
          labels   = if (is.null(leg_labels)) waiver() else leg_labels,
          na.value = ifelse(is.null(na.color),"grey50", na.color))
      }
    }

  } else {

    # ____________________________________________________________________________
    # Continuous palette: use scal_fill_brewer                                ####
    # TODO add support for scale_fill_viridis and maybe scale_fill_gradient

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
        trans    = ifelse(is.null(trans), "identity", trans)
      )
    }
  }

  plot
}
