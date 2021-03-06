#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_data PARAM_DESCRIPTION
#' @param fill_var PARAM_DESCRIPTION
#' @param facet_var PARAM_DESCRIPTION
#' @param facet_rows PARAM_DESCRIPTION, Default: NULL
#' @param borders_layer PARAM_DESCRIPTION, Default: NULL
#' @param borders_color PARAM_DESCRIPTION, Default: 'grey15'
#' @param borders_size PARAM_DESCRIPTION, Default: 0.2
#' @param borders_txt_fiels PARAM_DESCRIPTION, Default: NULL
#' @param borders_txt_size PARAM_DESCRIPTION, Default: 1
#' @param borders_txt_color PARAM_DESCRIPTION, Default: 'grey15'
#' @param basemap Yet to be implemented !!!!!, Default: NULL
#' @param zoomin PARAM_DESCRIPTION, Default: 0
#' @param xlims PARAM_DESCRIPTION, Default: NULL
#' @param ylims PARAM_DESCRIPTION, Default: NULL
#' @param zlims PARAM_DESCRIPTION, Default: NULL
#' @param zlims_type PARAM_DESCRIPTION, Default: 'vals'
#' @param outliers_style PARAM_DESCRIPTION, Default: 'recolor'
#' @param outliers_colors PARAM_DESCRIPTION, Default: c("grey10", "grey90")
#' @param scalebar PARAM_DESCRIPTION, Default: FALSE
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
#'  in_vect <- get_boundaries("ITA", level = 3) %>%
#'     dplyr::filter(NAME_1 == "Lombardia")
#'  geom <- in_vect %>%
#'    select(NAME_1, NAME_2, NAME_3)
#'  st_geometry(in_vect) <- NULL
#'  in_data <- in_vect  %>%
#'     dplyr::select(NAME_1, NAME_2, NAME_3) %>%
#'     dplyr::mutate(y_2003 = sample(dim(.)[1]),
#'                   y_2016 = 3*sample(dim(.)[1])) %>%
#'     tidyr::gather(year, population, -NAME_1, -NAME_2, -NAME_3)
#'  in_vect <- dplyr::left_join(in_data, geom) %>%
#'    sf::st_as_sf()
#'  plot_vect(in_vect,
#'            fill_var = "population",
#'            facet_var = "year",
#'            scalebar = TRUE,
#'            scalebar_dist = 30,
#'            theme = theme_light())
#'  }
#' }
#' @rdname plot_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom rosm osm.types
#' @importFrom scales squish censor
#' @importFrom sf st_transform
#' @importFrom grid unit
#' @importFrom methods is
#' @importFrom ggplot2 theme_bw fortify ggplot scale_x_continuous expand_scale
#'  scale_y_continuous ggtitle theme element_blank element_text element_rect
#'  geom_raster aes facet_wrap scale_fill_brewer scale_fill_distiller waiver
#'  geom_polygon scale_colour_manual guides guide_legend scale_color_manual
#'  coord_fixed
plot_vect_old <- function(
  in_data,
  fill_var       = NULL, fill_transparency = 0,
  polys_color    = "black", polys_size = 0.2,
  facet_var      = NULL, facet_rows     = NULL,
  borders_layer  = NULL, borders_color = "grey15", borders_size = 0.2,
  borders_txt_fiels = NULL, borders_txt_size = 1, borders_txt_color = "grey15",
  basemap        = NULL, zoomin = 0,
  xlims          = NULL, ylims = NULL,
  zlims          = NULL, zlims_type = "vals",
  outliers_style = "recolor", outliers_colors = c("grey10", "grey90"),
  scalebar       = FALSE, scalebar_dist = NULL,
  na.color       = NULL, na.value = NULL,
  palette_type   = "gradient", palette_name = NULL, direction = 1,
  leg_type       = NULL, leg_labels = NULL, leg_breaks = NULL,
  leg_position   = "right",
  no_axis        = FALSE, title = "Vector Plot", subtitle = NULL,
  theme          = theme_bw(),
  verbose        = TRUE
) {

  category <- NULL
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

  if (!is.null(facet_var)) {

    if (!class(in_data[[facet_var]]) %in% c("character", "factor")) {
      warning("The Specified `facet_var` is not of class `character` or",
              "`factor`. It will be ignored.")
      facet_var <- NULL
    }

  }

  # if (!is.null(basemap)) {
  #   assertthat::assert_that(
  #     basemap %in% rosm::osm.types(),
  #     msg = strwrap(
  #       "plot_rast_gg --> Invalid `basemap`. See `rosm::osm.types()` for
  #       valid value. Aborting!")
  #   )
  # }

  #   __________________________________________________________________________
  #   On NULL fill_var, just the geometry will be plotted                   ####

  if (is.null(fill_var)) {
    fill_var <- names(in_data)[1]
    in_data[[fill_var]] <- NA
    na.color <- "transparent"
  } else {
    # check the class of fill_var. If it is a factoror a character, palette_type
    # is reset to "categorical" regardless of user specification
    cls_fill_var <- class(in_data[[fill_var]])
    if (cls_fill_var %in% c("factor", "character")) {
      palette_type <- "categorical"
    }
  }

  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####

  palette_type <- switch(palette_type,
                         "categorical" = "qual",
                         "gradient"    = "seq",
                         "diverging"   = "div"
  )

  def_palettes  <- list(qual = "Set3",
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
      warning("plot_vect --> The selected palette name is not valid.\n",
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
        "plot_vect --> Invalid `leg_type`. It must be \"discrete\" or
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
  #   Reproject to 3857 to allow overlap with background map                ####

  # if (!is.null(basemap)) {
  #   if (verbose) {
  #     message("plot_rast_gg --> Reprojecting the input raster to epsg:3857")
  #   }
  #   in_data <- sf::st_transform(in_data, "+init=epsg:3857")
  # }

  #   __________________________________________________________________________
  #   set x/y limits                                                        ####

  if (is.null(xlims)) {
    xlims <- get_extent(in_data)@extent[c(1,3)]
  }

  if (is.null(ylims)) {
    ylims <- get_extent(in_data)@extent[c(2,4)]
  }

  #   _________________________________________________________________________
  #   if additional NA value passed, convert corresponding values of
  #   fill_var column to NA ####
  if (!is.null(na.value)) {
    in_data[which(in_data[fill_var] == na.value),][fill_var] <- NA
  }

  #   _________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering ####
  if (!is.null(na.color)) {
    if (na.color == "transparent")  {
      in_data <- in_data[!is.na(in_data[fill_var]),]
    }
  }

  if (!is.null(zlims) & zlims_type == "percs") {

    all_lims <- stats::quantile(in_data[[fill_var]], zlims, na.rm = TRUE)
    zlims <- c(min(all_lims[]), max(all_lims[2]))

  }

  # On continuous variables, perform processing to get the legends for outliers
  # "right"
  if (palette_type != "categorical") {
    if (!is.null(zlims)) {
      #   ____________________________________________________________________________
      #   If oob_style == "recolor" create additional data tables                 ####
      #   containing only values above / below limits
      if (outliers_style == "recolor") {

        out_low_tbl  <- in_data[which(in_data[[fill_var]] < zlims[1]),]
        out_high_tbl <- in_data[which(in_data[[fill_var]] > zlims[2]),]

        if (out_high_color == "transparent") {
          in_data <- in_data[which(in_data[[fill_var]] < zlims[2]),]
        }

        if (out_low_color == "transparent") {
          in_data <- in_data[which(in_data[[fill_var]] > zlims[1]),]
        }

      }
    }
  }
  #   __________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from longitude     ####
  #   range
  if (is.null(scalebar_dist)) {
    if (get_projunits(get_proj4string(in_data)) != "dec.degrees") {
      km_extent     <- round(diff(xlims)/1000)
      scalebar_dist <- round(km_extent/100*10)
    } else {
      deg2rad <- function(deg) {(deg * pi) / (180)}
      a <- sin(0.5 * (deg2rad(ylims[2]) - deg2rad(ylims[1])))
      b <- sin(0.5 * (deg2rad(xlims[2]) - deg2rad(xlims[1])))
      km_extent     <- 12742 * asin(sqrt(a * a +
                                           cos(deg2rad(ylims[1])) *
                                           cos(deg2rad(ylims[2])) * b * b))
      scalebar_dist <- round(km_extent/100*10)
      # scalebar_dist <- round(km_extent/100*10)
    }
  }

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

  #   __________________________________________________________________________
  #   add background map - need to do this here to prevent "shadowing"      ####
  #
  #   if (!is.null(basemap)) {
  #
  #     osm_plot <- suppressMessages(
  #       ggspatial::geom_osm(zoomin = zoomin, type = basemap, progress = "text")
  #     )
  #     vect <- fortify(as(in_data, "Spatial"))
  #     names(vect)[1:2] <- (c("x","y"))
  #     plot + osm_plot + geom_spatial(as(in_data, "Spatial"), aes(x = long, y = lat, fill = NAME_2))
  #
  #
  #     bmap <- get_map(sp::bbox(
  #       as(sf::st_transform(as(get_extent(in_data), "sfc_POLYGON"), 4326), "Spatial")
  #       ), maptype = "roadmap")
  #     plot <- plot + ggmap(bmap)
  #   }


  # Center the title - can be overriden in case after plot completion
  plot <- plot +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent"))

  # Add the sf layer
  plot <- plot + geom_sf(data = in_data,
                         aes_string(fill = fill_var),
                         size = polys_size,
                         color = polys_color,
                         alpha = 1 - fill_transparency) +
    coord_sf(xlim = xlims, ylim = ylims)

  #   __________________________________________________________________________
  #   Modify the palette according to variable type and palette             ####

  if (palette_type == "qual") {
    if (!palette_name == "hue") {
      plot <- plot +
        scale_fill_brewer(type = "qual", palette = palette_name,
                          na.value = ifelse(is.null(na.color),
                                            "grey50", na.color)) +
        theme(legend.justification = "center",
              legend.box.spacing = grid::unit(0.5,"points"))
    } else {
      plot <- plot +
        scale_fill_hue(na.value = ifelse(is.null(na.color),
                                         "grey50", na.color)) +
        theme(legend.justification = "center",
              legend.box.spacing = grid::unit(0.5,"points"))
    }
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



  # scale_fill_distiller(type = ifelse(palette_type == "sequential", "seq", "div"),
  #                      guide = ifelse(leg_type == "qual", "legend", "colourbar"),
  #                      palette = palette_name,
  #                      # labels = waiver(),
  #                      # limits = waiver(),
  #                      oob = scales::squish,
  #                      breaks = waiver(),
  #                      direction = 1,
  #                      na.value = "transparent") +
  plot <- plot + theme(plot.margin   = margin(0.1,0.1,0.1,0.1, unit = "cm"),
                       panel.ontop = T,
                       panel.background = element_rect(fill = "transparent"))

  if (leg_position == "bottom") {
    plot <- plot + theme(legend.position = "bottom")
    if (palette_type == "qual" | leg_type == "legend") {
      plot <- plot + guides(fill = guide_legend(title.position = "bottom",
                                                title.hjust = 0.5))
    } else {
      plot <- plot + guides(
        fill = guide_colourbar(title.position = "bottom",
                               title.hjust = 0.5))
    }
  }

  #   __________________________________________________________________________
  ##  If borders passed, add a "borders" layer                              ####

  if (!is.null(borders_layer)) {
    borders <- sf::st_transform(borders_layer, get_proj4string(in_data)) %>%
      crop_vect(in_data)
    plot    <- plot + geom_sf(data = borders,
                              fill    = "transparent",
                              color   = borders_color,
                              size    = borders_size)
    if (!is.null(borders_txt_fiels)) {
      if(borders_txt_fiels %in% names(borders)) {
        borders <- borders %>%
          dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                        lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
          ) %>%
          sf::st_as_sf()
        plot <- plot + geom_text(data = borders,
                                 aes_string(label = borders_txt_fiels,
                                            x = "lon", y = "lat"),
                                 size = borders_txt_size,
                                 color = borders_txt_color)
      }
    }
  }

  #   _________________________________________________________________________
  #   Facet the plot if a proper faceting variable was passed               ####

  if (!is.null(facet_var)) {
    in_data[[facet_var]] <- as.factor(in_data[[facet_var]])
    if (is.null(facet_rows)) {
      facet_rows <- floor(length(unique(in_data[[facet_var]])) / 2)
    }
    plot <- plot + facet_wrap(as.formula(paste("~", facet_var)),
                              nrow = facet_rows, drop = T)
  }


  #   ____________________________________________________________________________
  #   if oob_style = recolor, add "layers" corresponding to oob data          ####

  if (outliers_style == "recolor" & !is.null(zlims)) {

    if (length(unique(outliers_colors)) == 1) {

      dummy_data <- data.frame(out_high_tbl[1,]) %>%
        dplyr::mutate(cat = c("Out"),
                      color = c(out_high_color),
                      x = 0, y = 0 ) %>%
        sf::st_as_sf()



      plot <- plot + geom_polygon(data = dummy_data,
                                  aes(x = 0,
                                      y = 0,
                                      colour = color))
      plot <- plot + scale_colour_manual(
        "Outliers",
        values     = out_high_color,
        labels     = paste0("< ", format(zlims[1], digits = 2)
                            , " OR ", "> ", format(zlims[2], digits = 2))
      )

      plot <- plot +
        guides(colour = guide_legend(
          title = "Outliers", override.aes = list(fill = out_high_color,
                                                  color = "black")))
    } else {

      dummy_data <- data.frame(rbind(out_high_tbl[2,], out_low_tbl[1,])) %>%
        dplyr::mutate(cat = c("High","Low"),
                      color = c(out_low_color, out_high_color))
      plot <- plot + geom_polygon(data = dummy_data,
                                  aes(x = 0, y = 0,
                                      colour = color))
      plot <- plot + scale_color_manual(
        "Outliers",
        values     = c(out_low_color, out_high_color),
        labels     = c(paste0("< ", format(zlims[1], digits = 2)),
                       paste0("> ", format(zlims[2], digits = 2)))
      )
      plot <- plot +
        guides(colour = guide_legend(
          title = "Outliers",
          override.aes = list(fill = c(out_low_color, out_high_color),
                              color = "black")))
    }

    plot <- plot + geom_sf(data  = out_high_tbl,
                           fill  = out_high_color,
                           na.rm = TRUE)
    plot <- plot + geom_sf(data  = out_low_tbl,
                           fill  = out_low_color,
                           na.rm = TRUE)

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
