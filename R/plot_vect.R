#' @title plot a map based on an sf object
#' @description FUNCTION_DESCRIPTION
#' @param in_data PARAM_DESCRIPTION
#' @param fill_var PARAM_DESCRIPTION
#' @param facet_rows PARAM_DESCRIPTION, Default: NULL
#' @param borders_layer PARAM_DESCRIPTION, Default: NULL
#' @param borders_color PARAM_DESCRIPTION, Default: 'grey15'
#' @param borders_size PARAM_DESCRIPTION, Default: 0.2
#' @param xlims PARAM_DESCRIPTION, Default: NULL
#' @param ylims PARAM_DESCRIPTION, Default: NULL
#' @param zlims PARAM_DESCRIPTION, Default: NULL
#' @param zlims_type PARAM_DESCRIPTION, Default: 'vals'
#' @param outliers_style PARAM_DESCRIPTION, Default: 'recolor'
#' @param outliers_colors PARAM_DESCRIPTION, Default: c("grey10", "grey90")
#' @param scalebar PARAM_DESCRIPTION, Default: TRUE
#' @param scalebar_dist PARAM_DESCRIPTION, Default: NULL
#' @param na.color PARAM_DESCRIPTION, Default: NULL
#' @param na.value PARAM_DESCRIPTION, Default: NULL
#' @param palette_type PARAM_DESCRIPTION, Default: 'gradient'
#' @param palette_name PARAM_DESCRIPTION, Default: NULL
#' @param direction PARAM_DESCRIPTION, Default: 1
#' @param leg_type PARAM_DESCRIPTION, Default: NULL
#' @param leg_labels PARAM_DESCRIPTION, Default: NULL
#' @param leg_breaks PARAM_DESCRIPTION, Default: NULL
#' @param no_axis PARAM_DESCRIPTION, Default: FALSE
#' @param title PARAM_DESCRIPTION, Default: 'Vector Plot'
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @param theme PARAM_DESCRIPTION, Default: theme_bw()
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 theme_bw theme guides guide_colourbar geom_sf
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom scales squish
#' @importFrom sf st_transform
#' @importFrom methods is
plot_vect <- function(
  in_data,
  fill_var,
  facet_rows = NULL,
  borders_layer = NULL, borders_color = "grey15", borders_size = 0.2,
  xlims      = NULL, ylims = NULL,
  zlims      = NULL, zlims_type = "vals",
  outliers_style = "recolor", outliers_colors = c("grey10", "grey90"),
  scalebar     = TRUE, scalebar_dist = NULL,
  na.color     = NULL, na.value = NULL,
  palette_type = "gradient", palette_name = NULL, direction = 1,
  leg_type     = NULL, leg_labels = NULL, leg_breaks = NULL,
  no_axis      = FALSE, title = "Vector Plot", subtitle = NULL,
  theme        = theme_bw(),
  verbose      = TRUE
) {

  . <- N <- perc <- category <- NULL
  assertthat::assert_that(
    methods::is(in_data, "sf"),
    msg = "plot_vect --> `in_data` is not a valid `sf` object. Aborting!"
  )

  assertthat::assert_that(
    palette_type %in% c("categorical", "gradient", "diverging"),
    msg = strwrap(
      "plot_vect --> Invalid `palette_type`. It must be \"categorical\"
      \"gradient\" or \"diverging\". Aborting!")
  )

  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####

  palette_type <- switch(palette_type,
                         "categorical" = "qual",
                         "gradient"    = "seq",
                         "diverging"   = "div"
  )

  def_palettes  <- list(qual = "Set1",
                        seq  = "Greens",
                        div  = "RdYlGn")


  def_legtypes  <- list(qual = "discrete",
                        seq  = "continuous",
                        div  = "continuous")

  if (!is.null(palette_name)) {
    # Check validity of palette_name.
    # reset to default if palette_name not valid for selected palette_type
    all_pals <- cbind(name = row.names(RColorBrewer::brewer.pal.info),
                      RColorBrewer::brewer.pal.info)
    valid_pals <- subset(all_pals, category == palette_type)

    if (!palette_name %in% valid_pals$name) {
      warning("plot_rast_gg --> The selected palette name is not valid.\n",
              "Reverting to default value for selecter palette type (",
              as.character(def_palettes[palette_type]), ")")
      palette_name = as.character(def_palettes[palette_type])
    }
  } else {
    palette_name <- as.character(def_palettes[palette_type])
  }

  if (is.null(leg_type)) {
    leg_type <- as.character(def_legtypes[leg_type])
  }


  if (is.null(xlims)) {
    xlims <- get_extent(in_data)@extent[c(1,3)]
  }

  if (is.null(ylims)) {
    ylims <- get_extent(in_data)@extent[c(2,4)]
  }

  #   __________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from lonfgitude     ####
  #   range
  if (is.null(scalebar_dist)) {
    km_extent     <- round(diff(xlims)/1000)
    scalebar_dist <- round(km_extent/100*10)
  }


  plot <- ggplot()
  borders <- sf::st_transform(borders, get_proj4string(in_data))
  if (!is.null(borders)) plot <- plot + geom_sf(data = borders,
                                                fill = "grey95"
                                                , size = 0.3)

  plot <- plot + geom_sf(data = in_data, aes_string(fill = fill_var), size = 0.1) +
    # geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2.5) +
    xlim(get_extent(in_data)@extent[c(1,3)]) +
    ylim(get_extent(in_data)@extent[c(2,4)]) +
    ggplot2::theme_bw() +
    scale_fill_distiller(type = ifelse(palette_type == "sequential", "seq", "div"),
                         guide = ifelse(leg_type == "qual", "legend", "colourbar"),
                         palette = palette_name,
                         # labels = waiver(),
                         # limits = waiver(),
                         oob = scales::squish,
                         breaks = waiver(),
                         direction = 1,
                         na.value = "transparent") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(title = "",
                                      title.position = "bottom",
                                      title.hjust = 0.5,
                                      label.position = "top",
                                      label.hjust = 0.5,
                                      barwidth = unit(0.5, "npc"))) +
    # geom_text(data = borders, aes(label = NAME_1, x = lon, y = lat), size = 2) +
    ggplot2::geom_sf(data = borders, fill = "transparent", size = 0.2) +

    theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
                                                           color = "grey30"))  +
    theme(legend.margin = margin(0,0,0,0, unit = "cm")) +
    theme(plot.margin   = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.grid.major = element_line(color = "grey45", size = 0.5)) +
    labs(title = title) +
    theme(legend.box.spacing = unit(0, "mm"))
  # +
  #  geom_sf(data = river, color = "#378DD4", alpha = 0.1, size = 0.2)
  plot

}
