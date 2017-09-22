#' @title plot a map based on an sf object
#' @description FUNCTION_DESCRIPTION
#' @param in_data PARAM_DESCRIPTION
#' @param fill_var PARAM_DESCRIPTION
#' @param facet_rows PARAM_DESCRIPTION, Default: NULL
#' @param borders_layer PARAM_DESCRIPTION, Default: NULL
#' @param borders_color PARAM_DESCRIPTION, Default: 'grey15'
#' @param borders_size PARAM_DESCRIPTION, Default: 0.2
#' @param basemap `character` If not NULL and valid, the selected basemap is
#'   used as background. For a list of valid values, see `rosm::osm.types()`,
#'   Default: NULL
#' @param zoomin PARAM_DESCRIPTION, Default: 0
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
#' @importFrom ggplot2 theme_bw theme guides guide_colourbar geom_sf aes_string
#'   xlim ylim margin element_line labs
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom scales squish
#' @importFrom sf st_transform
#' @importFrom methods is
plot_vect <- function(
  in_data,
  fill_var,
  facet_rows     = NULL,
  borders_layer  = NULL, borders_color = "grey15", borders_size = 0.2,
  basemap        = NULL, zoomin = 0,
  xlims          = NULL, ylims = NULL,
  zlims          = NULL, zlims_type = "vals",
  outliers_style = "recolor", outliers_colors = c("grey10", "grey90"),
  scalebar       = FALSE, scalebar_dist = NULL,
  na.color       = NULL, na.value = NULL,
  palette_type   = "gradient", palette_name = NULL, direction = 1,
  leg_type       = NULL, leg_labels = NULL, leg_breaks = NULL,
  no_axis        = FALSE, title = "Vector Plot", subtitle = NULL,
  theme          = theme_bw(),
  verbose        = TRUE
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

  if (!is.null(basemap)) {
    assertthat::assert_that(
      basemap %in% rosm::osm.types(),
      msg = strwrap(
        "plot_rast_gg --> Invalid `basemap`. See `rosm::osm.types()` for
        valid value. Aborting!")
    )
  }

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


  def_legtypes  <- list(qual = "legend",
                        seq  = "colourbar",
                        div  = "colourbar")

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
    leg_type <- as.character(def_legtypes[palette_type])
  } else {
    assertthat::assert_that(
      leg_type %in% c("discrete", "continuous"),
      msg = strwrap(
        "plot_rast_gg --> Invalid `palette_type`. It must be \"discrete\" or
      \"continuous\". Aborting!")
    )
    leg_type <- switch(leg_type,
                       "discrete" = "legend",
                       "continuous"    = "colourbar"
    )
  }

  if (length(outliers_colors) == 1) {
    out_high_color <- out_low_color <- outliers_colors
  } else {
    out_high_color = outliers_colors[2]
    out_low_color = outliers_colors[1]
  }

  #   __________________________________________________________________________
  #   set x/y limits                                                        ####

  if (is.null(xlims)) {
    xlims <- get_extent(in_data)@extent[c(1,3)]
  }

  if (is.null(ylims)) {
    ylims <- get_extent(in_data)@extent[c(2,4)]
  }

  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  if (!is.null(basemap)) {
    if (verbose) {
      message("plot_rast_gg --> Reprojecting the input raster to epsg:3857")
    }
    in_data <- sf::st_transform(in_data, "+init=epsg:3857")
  }

  #   _________________________________________________________________________
  #   if additional NA value passed, convert corresponding values of
  #   fill_var column to NA ####
  if (!is.null(na.value)) {
    in_data <- in_data[which(in_data[[fill_var]] == na.value),][[fill_var]] = NA
  }

  #   _________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering ####
  if (!is.null(na.color)) {
    if (na.color == "transparent")  {
      in_data[!is.na(in_data[[fill_var]]),]
    }
  }

  if (!is.null(zlims) & zlims_type == "percs") {

    all_lims <- in_rast_fort[,  as.list(stats::quantile(value, zlims,
                                                        na.rm = TRUE)),
                             by = band]
    zlims <- c(min(all_lims[,2]), max(all_lims[,3]))

  }


  if (!is.null(zlims)) {
    #   ____________________________________________________________________________
    #   If oob_style == "recolor" create additional data tables                 ####
    #   containing only values above / below limits
    if (outliers_style == "recolor") {

      out_low_tbl  <- subset(in_rast_fort, value < zlims[1])
      out_high_tbl <- subset(in_rast_fort, value > zlims[2])

      if (out_high_color == "transparent") {
        in_rast_fort <- subset(in_rast_fort,
                               value < zlims[2] | is.na(value) == TRUE)
      }

      if (out_low_color == "transparent") {
        in_rast_fort <- subset(in_rast_fort,
                               value > zlims[1] | is.na(value) == TRUE)
      }

    }
  }
  #   __________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from longitude     ####
  #   range
  if (is.null(scalebar_dist)) {
    km_extent     <- round(diff(xlims)/1000)
    scalebar_dist <- round(km_extent/100*10)
  }
 browser()
 # Blank plot
  plot <- ggplot() + theme +
    scale_x_continuous(
                       expand = expand_scale(mult = c(0.005,0.005)),
                       limits = c(xlims[1], xlims[2])) +
    scale_y_continuous(
                       expand = expand_scale(mult = c(0.005,0.005)),
                       limits = c(ylims[1], ylims[2])) +
    ggtitle(title, subtitle = subtitle)

  # Remove axes if no_axis == TRUE
  if (no_axis) {

    plot <- plot  +
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
  }

  # Center the title - can be overriden in case after plot completion
  plot <- plot +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent"))

  # Add the sf layer
  plot <- plot + geom_sf(data = in_data, aes_string(fill = fill_var), size = 0.1) +
    # geom_text(data = borders, aes(label = NAME_2, x = lon, y = lat), size = 2.5) +
    coord_sf(xlim = xlims, ylim = ylims)

  #   __________________________________________________________________________
  #   Modify the palette according to variable type and palette             ####

  if (palette_type == "categorical") {
    plot <- plot +
      scale_fill_brewer(type = "qual", palette = palette_name)
  } else {

    plot <- plot +
      scale_fill_distiller(
        "Value",
        limits = zlims,
        breaks = if (is.null(leg_breaks)) {
          waiver()
        } else {
          leg_breaks
        }, labels = if (is.null(leg_labels)) {
          waiver()
        } else {
          leg_labels
        }, type = ifelse(palette_type == "sequential", "seq", "div"),
        guide = leg_type,
        palette = palette_name, oob = ifelse((outliers_style == "to_minmax"),
                                             scales::squish, scales::censor),
        direction = direction,
        na.value = ifelse(is.null(na.color), "grey50", na.color)) +
      theme(legend.justification = "center",
            legend.box.spacing = grid::unit(0.5,"points"))
  }



  scale_fill_distiller(type = ifelse(palette_type == "sequential", "seq", "div"),
                         guide = ifelse(leg_type == "qual", "legend", "colourbar"),
                         palette = palette_name,
                         # labels = waiver(),
                         # limits = waiver(),
                         oob = scales::squish,
                         breaks = waiver(),
                         direction = 1,
                         na.value = "transparent") +
    theme(plot.margin   = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.ontop = T,
          panel.background = element_rect(fill = "transparent",
                                          color = "grey30"))

    # ggplot2::theme(legend.position = "bottom") +
    # ggplot2::guides(
    #   fill = ggplot2::guide_colourbar(title = "",
    #                                   title.position = "bottom",
    #                                   title.hjust = 0.5,
    #                                   label.position = "top",
    #                                   label.hjust = 0.5,
    #                                   barwidth = unit(0.5, "npc"))) +
    # geom_text(data = borders, aes(label = NAME_1, x = lon, y = lat), size = 2) +
    #
    #     theme(panel.ontop = T, panel.background = element_rect(fill = "transparent",
  #                                                            color = "grey30"))  +
  #     theme(panel.grid.major = element_line(color = "grey45", size = 0.5))
  #     # theme(legend.margin = margin(0.2,0.2,0.2,0.2, unit = "cm")) +
  #     theme(plot.margin   = margin(0.1,0.1,0.1,0.1, unit = "cm"),
  #           panel.grid.major = element_line(color = "grey45", size = 0.5)) +

  # +
  #   theme(legend.box.spacing = unit(0.1, "mm"))

  #   __________________________________________________________________________
  ##  If borders passed, add a "borders" layer                              ####

  if (!is.null(borders_layer)) {
    borders <- sf::st_transform(borders_layer, get_proj4string(in_data))
    plot    <- plot + geom_sf(data = borders,
                              fill    = "transparent",
                              color   = borders_color,
                              size    = borders_size)
  }

  #   _________________________________________________________________________
  #   Add scalebar                                                          ####

  if (scalebar) {
    # coord_cartesian(xlim = xlims, ylim = ylims) +
    plot <- plot +
      sprawl_scalebar(dd2km = TRUE, dist = scalebar_dist,
                      x.min = xlims[1], x.max = xlims[2],
                      y.min = ylims[1], y.max = ylims[2],
                      location = "bottomright", st.size = 3.5,
                      st.bottom = FALSE, model = NULL,
                      st.dist = 0.025, units = "km")
  }

  plot

}
