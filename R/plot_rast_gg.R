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
#' @param oob_style `character` specifies how the values outside of the
#'   zlims range will be pllotted. If == NULL (the default), out of bounds values
#'   are plotted using the color specified in "oob.color". If == "expand",
#'   they are plotted using the minimum and maximum colours of the scale (using)
#'   `scales::squish`
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
#' (e.g., ggplot2::theme_light()), Default: ggplot2::theme_bw()
#' @param verbose `logical`, If FALSE, suppress processing message,
#'  Default: TRUE
#' @param title `character`, Title of the plot, Default: NULL
#' @param subtitle Subtitle of the plot, Default: NULL
#' @param theme `theme function` ggplot theme to be used
#' (e.g., ggplot2::theme_light()), Default: ggplot2::theme_bw()
#' @param verbose `logical`, If FALSE, suppress processing messages,
#'  Default: TRUE
#' @return a `ggplot`
#' @examples
#' \dontrun{
#' #single band plot
#'  in_rast <- raster::stack(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm",
#'                   palette_type = "diverging",
#'                   na.value = 0,
#'                   title = "OLI", subtitle = "Band 2")
#'
#'  #Change basemap and transparency
#'  plot_rast_gg(in_rast, basemap = "stamenbw",
#'                   palette_type = "diverging",
#'                   palette_name = "RdYlBu",
#'                   no_axis = T,
#'                   na.value = 0, transparency = 0.2,
#'                   title = "OLI - 15/06/2017",
#'                   subtitle = "Band 2 - Green")
#'
#' #Multi band plot with selection of bands
#'  in_rast <- raster::stack(system.file("extdata/OLI_test",
#'   "oli_multi_1000.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm", bands_to_plot = c(1,4),
#'                   palette_type = "diverging",
#'                   na.value = 0,
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
#'  # limits adjusted on the 1st and 100th percentile to remove very low
#'  # values outliers and better use the palette
#'  plot_rast_gg(in_rast,
#'              palette_type = "diverging", palette_name = "RdYlBu",
#'              zlims = c(0.01, 1.0), zlims_type = "percs",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye")
#' }
#' @rdname plot_rast_gg
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter
#' @importFrom gdalUtils gdalwarp
#' @importFrom ggplot2 theme_bw fortify ggplot scale_x_continuous
#'   scale_y_continuous ggtitle theme element_blank element_text geom_raster
#'   aes scale_fill_brewer scale_fill_distiller waiver coord_fixed facet_wrap
#' @importFrom ggspatial geom_osm
#' @importFrom raster stack
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom rosm osm.types
#' @importFrom grid unit
#' @importFrom stats na.omit
#' @importFrom magrittr "%>%"

plot_rast_gg <- function(
  in_rast,
  band_names   = NULL, bands_to_plot = NULL, facet_rows = NULL,
  xlims        = NULL, ylims = NULL,
  zlims        = NULL, zlims_type = "vals",
  oob_style    = NULL, oob_color = "purple",
  basemap      = NULL, zoomin = 0,
  scalebar     = TRUE, scalebar_dist = NULL,
  transparency = 0,
  na.color     = NULL, na.value = NULL,
  palette_type = "gradient", palette_name = NULL,
  leg_type     = NULL, leg_labels = NULL, leg_breaks = NULL,
  no_axis      = FALSE, title = NULL, subtitle = NULL,
  theme        = ggplot2::theme_bw(),
  verbose      = TRUE
) {

  #TODO find a way to avoid "require"
  #TODO Implement checks on input arguments (e.g., bands_to_plot, band_names)
  #TODO Verify possibility to have a "satellite" basemap
  #TODO Add MaxPixels
  loadNamespace("ggspatial")
  loadNamespace("ggplot2")
  x <- y <- value <- band <- category <- NULL

  assertthat::assert_that(palette_type %in% c("categorical", "gradient", "diverging"), #nolint
                          msg = "plot_rast_gg --> Invalid palette_type. Aborting!"
  )

  if (!is.null(basemap)) {
    assertthat::assert_that(basemap %in% rosm::osm.types(),
                            msg = "plot_rast_gg --> Invalid basemap name. Aborting!")
  }
  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####

  palette_type <- switch(palette_type,
                         "categorical" = "qual",
                         "gradient"    = "seq",
                         "diverging"   = "div"
  )

  if (is.null(palette_type)) {
    stop("plot_rast_gg --> Invalid palette type. Aborting")
  }

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
              " Reverting to default value for selecter palette type (",
              as.character(def_palettes[palette_type]), ")")
      palette_name = as.character(def_palettes[palette_type])
    }
  } else {
    palette_name <- as.character(def_palettes[palette_type])
  }

  if (is.null(leg_type)) {
    leg_type <- as.character(def_legtypes[palette_type])
  }

  if (!is.null(bands_to_plot)) {

    in_rast <- in_rast[[bands_to_plot]]

  }

  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  rastinfo <- get_rastinfo(in_rast, verbose = FALSE)

  if (is.null(facet_rows)) {
    facet_rows <- rastinfo$nbands
  }

  if (any(rastinfo$fnames == "")) {
    rastfile <- cast_rast(in_rast, "rastfile")
    in_rast <- raster::stack(rastfile)
  }
  rastinfo$fnames <- get_rastinfo(in_rast, verbose = FALSE)$fnames

  if (!is.null(basemap)) {

    temp_vrt <- tempfile(fileext = ".vrt")
    tmp_txt <- tempfile(fileext = ".txt")
    writeLines(rastinfo$fnames, tmp_txt)
    buildvrt_string <- paste(paste(paste("-b ", rastinfo$indbands),
                                   collapse = " "),
                             "-input_file_list",
                             tmp_txt,
                             temp_vrt)

    system2(file.path(find_gdal(), "gdalbuildvrt"),
            args = buildvrt_string,
            stdout = NULL)

    if (verbose) {
      message("plot_rast_gg --> Reprojecting the input raster to epsg:3857")
    }
    in_rast <- gdalUtils::gdalwarp(temp_vrt,
                                   tempfile(fileext = ".tif"),
                                   s_srs = rastinfo$proj4string,
                                   t_srs = "+init=epsg:3857",
                                   output_Raster = TRUE,
                                   tr = rastinfo$res,
                                   overwrite = T)
  }


  #   ____________________________________________________________________________
  #   Fortify the raster to allow ggplotting                                  ####

  in_rast_fort <- ggplot2::fortify(in_rast, format = "long") %>%
    data.table::as.data.table()

  #   __________________________________________________________________________
  #   If band_names not passed use the band names found in the file - these are
  #   used to set the strip labels in case of faceted plot !

  if (is.null(band_names)) {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = rastinfo$bnames)
  } else {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = band_names)
  }

  if (!is.null(na.value)) {
    in_rast_fort[value == na.value] <- NA
  }

  #   ____________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering   ####
  if (!is.null(na.color)){
    if (na.color == "transparent")  {
      in_rast_fort <- na.omit(in_rast_fort, "value")
    }
  }
  if (!is.null(zlims) & zlims_type == "percs") {

    all_lims <- in_rast_fort[,  as.list(quantile(value, zlims, na.rm=TRUE)),
                             by = band]
    zlims <- as.numeric(all_lims[,2:3])
  }


  #   ____________________________________________________________________________
  #   If no limits passed, compute them from the input extent                 ####

  if (is.null(xlims)) {
    xlims <- c(min(in_rast_fort$x), max(in_rast_fort$x))
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

  if (palette_type == "categorical") {

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
  plot_gg <- ggplot2::ggplot(in_rast_fort) + theme +
    ggplot2::scale_x_continuous(xlab,
                                expand = expand_scale(mult = c(0.005,0.005)),
                                limits = c(xlims[1], xlims[2])) +
    ggplot2::scale_y_continuous(ylab,
                                expand = expand_scale(mult = c(0.005,0.005)),
                                limits = c(ylims[1], ylims[2])) +
    ggplot2::ggtitle(title, subtitle = subtitle)

  # Remove axes if no_axis == TRUE
  if (no_axis) {

    plot_gg <- plot_gg  +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  # Center the title - can be overriden in case after plot completion
  plot_gg <- plot_gg +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

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
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = value),
                         alpha = 1 - transparency)

  #   __________________________________________________________________________
  #   Modify the palette according to variable type and palette             ####

  if (palette_type == "categorical") {
    plot_gg <- plot_gg +
      ggplot2::scale_fill_brewer(type = "qual", palette = palette_name)
  } else {

    plot_gg <- plot_gg +
      ggplot2::scale_fill_distiller(
        "",
        limits = zlims, breaks = if (is.null(leg_breaks)) {
          ggplot2::waiver()
        } else {
          leg_breaks
        }, labels = if (is.null(leg_labels)) {
          ggplot2::waiver()
        } else {
          leg_labels
        }, type = ifelse(palette_type == "sequential", "seq", "div"),
        guide = ifelse(leg_type == "qual", "legend", "colourbar"),
        palette = palette_name, oob = ifelse(is.null(oob_style), censor, squish),
        direction = 1,
        na.value = ifelse(is.null(na.color), "grey50", na.color)) +
      theme(legend.justification = "center",
            legend.box.spacing = grid::unit(0.5,"points"))
  }

  #   ____________________________________________________________________________
  #   Add scalebar               ####

  if (scalebar) {
    # ggplot2::coord_cartesian(xlim = xlims, ylim = ylims) +
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
    ggplot2::coord_fixed()

  if (rastinfo$nbands > 1) {
    plot_gg <- plot_gg + ggplot2::facet_wrap(~band, nrow = facet_rows)
  }
  plot_gg
}

