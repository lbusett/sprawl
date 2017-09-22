#' @title Plot a raster object using ggplot
#' @description Plot a raster object using ggplot witn an (optional) basemap.
#'   The function allows to:
#'   - Plot a single- or multi-band image using facet_wrap (args `bands_to_plot`,
#'    `facet_rows`)
#'   - Add an optional basemap to the plot, and adjust transparency of the
#'     overlayed raster (args `basemap`, `transparency`)
#'   - "Easily" select different palettes (args `palette_type`, `palette_name`)
#'   - Easily control plotting limits on the z-dimension, specifying either
#'     values or quantiles ranges (args `zlims`, `zlims_type`)
#'   - "Easily" control breaks and labels in legends (args `leg_type`, `leg_breaks`,
#'     `leg_labels`)
#'   - Automatically add a scalebar to the plot (args`scalebar`, `scalebar_dist`)
#'
#' See the description of the arguments for details on their use
#' @param in_rast `Raster` object to be plotted. Both mono- and multi-band
#'   rasters are supported
#' @param maxpixels `numeric` maximum number of pixels to be used for plotting *for
#'   each band*. Reduce this to speed-up plotting by subsampling the raster (this
#'   reduces qualitty!). Increase it to improve qualitty (this reduces rendering
#'   speed!), Default:1e5
#' @param band_names `character(nbands)`, array of band names. These will used
#'   to populate the "strips" above each plotted band. If NULL, bnames are
#'   retrieved from the input raster using `sprawl::get_rastinfo`, Default: NULL
#' @param bands_to_plot `numeric array`, array of band numbers to be plotted (in
#'   case `in_rast` is multi-band. If NULL, all bands are plotted separately
#'   using facet_wrap, Default: NULL
#' @param facet_rows `numeric`, number of rows used for plotting multiple bands,
#'   If NULL, one row for each band is used (to be changed !), Default: NULL
#' @param xlims `numeric(2)`, minimum and maximum x coordinates to be plotted.
#'   If NULL, the whole x-range is plotted, Default: NULL
#' @param ylims `numeric(2)`, minimum and maximum y coordinates to be plotted.
#'   If NULL, the whole y-range is plotted, Default: NULL
#' @param zlims `numeric array [2]` limits governing the range of
#'  values to be plotted (e.g., c(0.2,0.4)), Default: NULL
#' @param zlims_type `character` type of zlims specified.
#'   - "vals": zlims indicates the range of values to be plotted
#'   - "percs": zlims indicates the range of percentiles to be plotted (e.g.,
#'      specifying zlims = c(0.02, 0.98), zlim_type = "percs" will plot the
#'      values between the 2nd to 98th percentile)
#' @param outliers_style `character ["as_na" / "recolor" / "to_minmax"]` specifies how
#'   the values outside of the zlims range will be plotted.
#'   - If == "as_na" (the default), out of bounds values are plotted using the
#'     color specified for na.color;
#'   - If == "recolor", they are plotted using the oob_high_color and oob_low_color
#'     arguments
#'   - If == "to_minmax" they are plotted using the minimum and maximum colours
#'     of the current palette (using scales::squish())
#'   `scales::squish`
#' @param outliers_colors `character (1/2)` specifies colors to be used to plot
#'   values outside zlims. If only one color is passed, both values above max(zlims)
#'   are plotted with the same color. If two colors are passed, the first color
#'   is used to plot values <. min(zlims) and the second to plot colors > max(zlims)
#' @param basemap `character` If not NULL and valid, the selected basemap is
#'   used as background. For a list of valid values, see `rosm::osm.types()`,
#'   Default: NULL
#' @param zoomin `numeric`, Adjustment factor for basemap zoom. Negative values
#'   lead to less detailed basemap, but larger text. Default: 0
#' @param scalebar `logical` If TRUE, add a scalebar on the bottom right corner,
#'   Default: TRUE
#' @param scalebar_dist `numeric` Width of the scale bar (in km). If NULL, it
#'   is computed automatically on the basis of the range in x direction,
#'   Default: NULL
#' @param transparency `numeric [0,1]`, transparency of the raster layer. Higer
#'   values lead to higher transparency, Default: 0 (ignored if basemap == NULL)
#' @param na.color `character`, color to be used to plot NA values,
#'   Default: 'grey50'. TO set to "invisible" use na.color = "transparent"
#' @param na.value `numeric`, Additional values to be treated as NA, Default: NA
#' @param palette_type `character` Type of brewer color ramp to be used. Possible
#'  values are `gradient`, `diverging` and `categorical`, Default: 'gradient'
#' @param palette_name name of the palette to be used. If NULL, the following
#'  defaults are used as a function of palette_type:
#'  - categorical --> "Set1"
#'  - gradient    --> "Greens"
#'  - diverging   --> "RdYlGn")
#'  Note that if a wrong palette name is specified, plot_rast_gg reverts to
#'  the default values. Run `RColorBrewer::display.brewer.all()` to see a list
#'  of available palettes, Default: NULL
#' @param direction DESCRIPTION NEEDEDD
#' @param leg_type DESCRIPTION NEEDEDD
#' @param leg_labels `character (n_leg_breaks)` labels to be used in the legend
#'   - If palette_type is "categorical", the number of labels must correspond to
#'   the number of unique values of the raster to be plotted. If NULL or not valid,
#'   the legend will use the unique raster values in the legend (see examples)
#'   - If palette_type is "gradient" or "diverging" the number of labels must be
#'     equal to the number of breaks specified by "leg_breaks". If this is not
#'     TRUE, leg_breaks and leg_labels are reset to `waiver()` (TBD),
#'     Default: NULL (the default ggplot2 `labels = waiver()` is used)
#' @param leg_breaks `numeric (n_leg_labels)` Values in the scale at which
#'   leg_labels must be placed (if palette_type != "categorical"). The number
#'   of breaks must be equal to the number of labels specified by "leg_labels".
#'   If this is not TRUE, leg_breaks and leg_labels are reset to `waiver()`
#'   (TBD)  Default: NULL (the default ggplot2 `labels = waiver()` is used)
#' @param no_axis `logical`, If TRUE, axis names and labels are suppressed,
#'   Default: FALSE
#' @param title `character`, Title of the plot, Default: NULL
#' @param subtitle Subtitle of the plot, Default: NULL
#' @param theme `theme function` ggplot theme to be used
#' (e.g., theme_light()), Default: theme_bw()
#' @param verbose `logical`, If FALSE, suppress processing message,
#'  Default: TRUE
#' @return a `ggplot`
#' @examples
#' \dontrun{
#' #single band plot
#'  in_rast <- read_rast(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm",
#'                   palette_type = "diverging",
#'                   na.value = 0, na.color = "transparent",
#'                   title = "OLI", subtitle = "Band 2")
#'
#'  #Change basemap and transparency
#'  plot_rast_gg(in_rast, basemap = "stamenbw",
#'                   palette_type = "diverging",
#'                   palette_name = "RdYlBu",
#'                   no_axis = T,
#'                   na.value = 0, na.color = "transparent",
#'                   transparency = 0.2,
#'                   title = "OLI - 15/06/2017",
#'                   subtitle = "Band 2 - Green")
#'
#' #Multi band plot with selection of bands
#'  in_rast <- raster::stack(system.file("extdata/OLI_test",
#'   "oli_multi_1000.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm", bands_to_plot = c(1,4),
#'                   palette_type = "diverging",
#'                   na.value = 0, na.color = "transparent",
#'                   title = "OLI", subtitle = "Band 2 vs band 4",
#'                   facet_row = 1)
#'
#' #Adjusting limits and changing palette
#'  in_rast <- raster::stack(system.file("extdata/REYE_test",
#'                           "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data"))
#'  #vanilla
#'  plot_rast_gg(in_rast,
#'              palette_type = "diverging", palette_name = "RdYlBu",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye")
#'
#'  # limits adjusted on the 10th and 99.9th percentile to remove very low
#'  # values outliers and better use the palette on high values (note that
#'  # outliers can be set as transparent!) + increase maxpixels to plot at
#'  # full resolution.
#'  plot_rast_gg(in_rast, basemap = "osm", maxpixels = 100e5,
#'              palette_type = "diverging", palette_name = "RdYlBu",
#'              zlims = c(0.10, 0.999), zlims_type = "percs",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
#'              outliers_style = "recolor",
#'              outliers_colors = c("transparent", "purple"))
#'
#'  # Adjust `maxpixels` to speed-up rendering by sacrificing quality
#'  plot_rast_gg(in_rast, basemap = "osm", maxpixels = 10e4,
#'              palette_type = "diverging", palette_name = "RdYlBu",
#'              zlims = c(0.10, 0.999), zlims_type = "percs",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
#'              outliers_style = "recolor",
#'              outliers_colors = c("transparent", "purple"),
#'              leg_type = "discrete")
#' }
#' @rdname plot_rast_gg
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom gdalUtils gdalwarp
#' @importFrom ggplot2 theme_bw fortify ggplot scale_x_continuous expand_scale
#'  scale_y_continuous ggtitle theme element_blank element_text element_rect
#'  geom_raster aes facet_wrap scale_fill_brewer scale_fill_distiller waiver
#'  geom_polygon scale_colour_manual guides guide_legend scale_color_manual
#'  coord_fixed
#' @importFrom ggspatial geom_osm
#' @importFrom raster stack sampleRegular nlayers
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom rosm osm.types
#' @importFrom scales squish censor
#' @importFrom grid unit
#' @importFrom stats na.omit
#' @importFrom methods is

plot_rast_gg <- function(
  in_rast,
  maxpixels    = 1e5,
  band_names   = NULL, bands_to_plot = NULL, facet_rows = NULL,
  xlims        = NULL, ylims = NULL,
  zlims        = NULL, zlims_type = "vals",
  outliers_style = "recolor", outliers_colors = c("grey10", "grey90"),
  basemap      = NULL, zoomin = 0,
  scalebar     = TRUE, scalebar_dist = NULL,
  transparency = 0,
  na.color     = NULL, na.value = NULL,
  palette_type = "gradient", palette_name = NULL, direction = 1,
  leg_type     = NULL, leg_labels = NULL, leg_breaks = NULL,
  no_axis      = FALSE, title = NULL, subtitle = NULL,
  theme        = theme_bw(),
  verbose      = TRUE
) {

  #TODO find a way to avoid "require"
  #TODO Implement checks on input arguments (e.g., bands_to_plot, band_names)
  #TODO Verify possibility to have a "satellite" basemap

  loadNamespace("ggspatial")
  loadNamespace("ggplot2")
  x <- y <- value <- band <- category <- color <- NULL


  #   __________________________________________________________________________
  #   check arguments                                                       ####

  assertthat::assert_that(
    methods::is(in_rast, "Raster"),
    msg = "plot_rast_gg --> `in_rast` is not a valid `Raster` object. Aborting!"
  )

  assertthat::assert_that(
    palette_type %in% c("categorical", "gradient", "diverging"),
    msg = strwrap(
      "plot_rast_gg --> Invalid `palette_type`. It must be \"categorical\"
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

  if (!is.null(bands_to_plot)) {

    in_rast <- in_rast[[bands_to_plot]]

  }

  if (length(outliers_colors) == 1) {
    out_high_color <- out_low_color <- outliers_colors
  } else {
    out_high_color = outliers_colors[2]
    out_low_color = outliers_colors[1]
  }


  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  rastinfo <- get_rastinfo(in_rast, verbose = FALSE)

  if (is.null(facet_rows)) {
    facet_rows <- round(rastinfo$nbands / 2)
  }

  if (any(rastinfo$fnames == "")) {
    rastfile <- cast_rast(in_rast, "rastfile")
    in_rast  <- raster::stack(rastfile)
  }
  rastinfo$fnames <- get_rastinfo(in_rast, verbose = FALSE)$fnames

  if (!is.null(basemap)) {
    if (verbose) {
      message("plot_rast_gg --> Reprojecting the input raster to epsg:3857")
    }
    in_rast <- reproj_rast(in_rast, "+init=epsg:3857")
  }

  #   __________________________________________________________________________
  #   Subsample if needed to speed-up plotting                              ####
  in_rast <-  raster::sampleRegular(
    x        = in_rast,
    size     = maxpixels * ifelse(is.null(bands_to_plot),
                                  raster::nlayers(in_rast),
                                  length(bands_to_plot)),
    asRaster = TRUE)

  #   ____________________________________________________________________________
  #   Fortify the raster to allow ggplotting                                  ####

  in_rast_fort <- fortify(in_rast, format = "long") %>%
    data.table::as.data.table()
  names(in_rast_fort)[1:2] <- c("x", "y")


  #   __________________________________________________________________________
  #   If band_names not passed use the band names found in the file - these are
  #   used to set the strip labels in case of faceted plot !

  if (is.null(band_names)) {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = rastinfo$bnames)
  } else {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = band_names)
  }

  if (!is.null(na.value)) {
    in_rast_fort[value == na.value, value := NA]
  }

  #   _________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering ####
  if (!is.null(na.color)) {
    if (na.color == "transparent")  {
      in_rast_fort <- stats::na.omit(in_rast_fort, "value")
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
  #   If no geo limits passed, compute them from the input extent           ####

  if (is.null(xlims)) {
    xlims <- c(min(in_rast_fort$x), max(in_rast_fort$x))
  }

  if (is.null(ylims)) {
    ylims <- c(min(in_rast_fort$y), max(in_rast_fort$y))
  }


  #   __________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from lonfgitude     ####
  #   range
  if (is.null(scalebar_dist)) {
    km_extent     <- round(diff(xlims)/1000)
    scalebar_dist <- round(km_extent/100*10)
  }

  #   __________________________________________________________________________
  #   If palette is categorical and leg_labels was passed, set the levels   ####
  #   of the categorical variables using leg_labels. Otherwis, the numeric
  #   value of the variable will be used !

  if (palette_type == "qual") {

    if (is.null(leg_labels)) {
      in_rast_fort[[4]] <- factor(in_rast_fort[[4]])
    } else {
      in_rast_fort[[4]] <- factor(in_rast_fort[[4]], labels = leg_labels)
    }
  }

  #   ____________________________________________________________________________
  #   Initialize the plot                                                     ####

  # set axes names
  if (rastinfo$units == "dec.degrees") {
    xlab = "Longitude"
    ylab = "Latitude"
  } else {
    xlab = paste0("Easting [",rastinfo$units,"]")
    ylab = paste0("Northing [",rastinfo$units,"]")
  }

  # Blank plot
  plot_gg <- ggplot() + theme +
    scale_x_continuous(xlab,
                       expand = expand_scale(mult = c(0.005,0.005)),
                       limits = c(xlims[1], xlims[2])) +
    scale_y_continuous(ylab,
                       expand = expand_scale(mult = c(0.005,0.005)),
                       limits = c(ylims[1], ylims[2])) +
    ggtitle(title, subtitle = subtitle)

  # Remove axes if no_axis == TRUE
  if (no_axis) {

    plot_gg <- plot_gg  +
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
  }

  # Center the title - can be overriden in case after plot completion
  plot_gg <- plot_gg +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent"))


  #   __________________________________________________________________________
  #   add background map - need to do this here to prevent "shadowing"      ####

  if (!is.null(basemap)) {

    osm_plot <- suppressMessages(
      ggspatial::geom_osm(zoomin = zoomin, type   = basemap, progress = "none")
    )
    plot_gg <- plot_gg + osm_plot
  }

  #   __________________________________________________________________________
  #   add the raster layer                                                  ####

  plot_gg <- plot_gg +
    geom_raster(data = in_rast_fort, aes(x, y, fill = value),
                alpha = 1 - transparency, na.rm = TRUE)
  if (rastinfo$nbands > 1) {
    plot_gg <- plot_gg + facet_wrap(~band, nrow = facet_rows, drop = T)
  }
  #   __________________________________________________________________________
  #   Modify the palette according to variable type and palette             ####

  if (palette_type == "categorical") {
    plot_gg <- plot_gg +
      scale_fill_brewer(type = "qual", palette = palette_name)
  } else {

    plot_gg <- plot_gg +
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

  #   ____________________________________________________________________________
  #   if oob_style = recolor, add "layers" corresponding to oob data          ####

  if (outliers_style == "recolor" & !is.null(zlims)) {

    if (length(unique(outliers_colors)) == 1) {

      dummy_data <- data.frame(out_high_tbl[1,]) %>%
        dplyr::mutate(cat = c("Out"),
                      color = c(out_high_color),
                      band = levels(in_rast_fort$band)[1])
      plot_gg <- plot_gg + geom_polygon(data = dummy_data,
                                        aes(x = x,
                                            y = y,
                                            colour = color))
      plot_gg <- plot_gg + scale_colour_manual(
        "Outliers",
        values     = out_high_color,
        labels     = paste0("< ", format(zlims[1], digits = 2)
                            , " OR ", "> ", format(zlims[2], digits = 2))
      )

      plot_gg <- plot_gg +
        guides(colour = guide_legend(
          title = "Outliers", override.aes = list(fill = out_high_color,
                                                  color = "black")))
    } else {

      dummy_data <- data.frame(rbind(out_high_tbl[2,], out_low_tbl[1,])) %>%
        dplyr::mutate(cat = c("High","Low"),
                      color = c(out_low_color, out_high_color),
                      band = levels(in_rast_fort$band)[1])
      plot_gg <- plot_gg + geom_polygon(data = dummy_data,
                                        aes(x = x, y = y,
                                            colour = color))
      plot_gg <- plot_gg + scale_color_manual(
        "Outliers",
        values     = c(out_low_color, out_high_color),
        labels     = c(paste0("< ", format(zlims[1], digits = 2)),
                       paste0("> ", format(zlims[2], digits = 2)))
      )
      plot_gg <- plot_gg +
        guides(colour = guide_legend(
          title = "Outliers",
          override.aes = list(fill = c(out_low_color, out_high_color),
                              color = "black")))
    }

    plot_gg <- plot_gg + geom_raster(data = out_high_tbl,
                                     aes(x = x, y = y),
                                     fill = out_high_color, na.rm = TRUE)
    plot_gg <- plot_gg + geom_raster(data = out_low_tbl,
                                     aes(x = x, y = y),
                                     fill = out_low_color,
                                     na.rm = TRUE)

  }


  #   _________________________________________________________________________
  #   Add scalebar                                                          ####

  if (scalebar) {
    # coord_cartesian(xlim = xlims, ylim = ylims) +
    plot_gg <- plot_gg +
      sprawl_scalebar(dd2km = FALSE, dist = scalebar_dist,
                      x.min = xlims[1], x.max = xlims[2],
                      y.min = ylims[1], y.max = ylims[2],
                      location = "bottomright", st.size = 3.5,
                      st.bottom = FALSE, model = NULL,
                      st.dist = 0.025, units = rastinfo$units)
  }

  #   __________________________________________________________________________
  #   Finalize the plot and return it                                       ####

  plot_gg <- plot_gg +
    coord_fixed()


  plot_gg
}

