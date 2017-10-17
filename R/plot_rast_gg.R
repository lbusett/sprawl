#' @title Plot a map based on a `raster` object using ggplot
#' @description Plot a raster object using ggplot with an (optional) basemap.
#'   The function allows to:
#'   - Plot a single- or multi-band image using facet_wrap (args `bands_to_plot`,
#'    `facet_rows`)
#'   - Add an optional basemap to the plot, and adjust transparency of the
#'     overlayed raster (args `basemap`, `transparency`)
#'   - "Easily" select different palettes (arg `palette_name`)
#'   - Easily control plotting limits on the z-dimension, specifying either
#'     values or quantiles ranges (args `zlims`, `zlims_type`), as well as how
#'     the outlier values are represented (args `outliers_style` and
#'     `outliers_color`)
#'   - Easily control breaks and labels in legends (args `leg_type`, `leg_breaks`,
#'     `leg_labels`)
#'   - Automatically add a scalebar to the plot (args`scalebar`, `scalebar_dist`,
#'     `scalebar_txt_dist`, `scalebar_txt_size`)
#'   - Easily add an additional "vector" layer (e.g., administrative boundaries)
#'     (args`borders_layer`, `borders_color`, `borders_....`)
#' See the description of the arguments for details on their use
#' @param in_rast `Raster` object to be plotted. Both mono- and multi-band
#'   rasters are supported;
#' @param rast_type `character` ("continuous" | "categorical"). Specifies if the
#'   data in `in_rast` correspond to a continuous or categorical (i.e., low number
#'   of integer levels - typically a classified raster) variable.
#'   If NULL, the function tries to devise the correct type from the data,
#'    Default: NULL
#' @param palette_name `character` name of the palette to be used to "color" the raster.
#'  If NULL, the following defaults are used as a function of variable type:
#'  - categorical --> "hue"
#'  - continuous  --> "Greys"
#'  Note that if a wrong palette name is specified, plot_rast_gg reverts to
#'  the default values. Run `sprawl::fillpals()` to see a list
#'  of available palettes, Default: NULL
#' @param direction `character [0 | 1]` direction of the color legend. Change this
#'  to invert the color gradient, Default: 1
#' @param leg_type `character ["continuous", "discrete"]` type of legend to be used
#'  on continuous variables. If "continuous" , a colourbar is used. If "discrete",
#'  a discretized version is used (see examples).#'
#' @param leg_labels `character (n_leg_breaks)` labels to be used for the legend
#'   - If `rast_type` == "categorical", the number of labels must correspond to
#'   the number of unique values of the raster to be plotted. If NULL or not valid,
#'   the legend will use the unique raster values in the legend (see examples)
#'   - If `rast_type` ==  "continuous" the number of labels must be
#'     equal to the number of breaks specified by "leg_breaks". If this is not
#'     TRUE, `leg_breaks` and `leg_labels` are reset to `waiver()` (TBD),
#'     Default: NULL (the default ggplot2 `labels = waiver()` is used)
#' @param leg_breaks `numeric (n_leg_labels)` Values in the scale at which
#'   leg_labels must be placed (if rast_type != "categorical"). The number
#'   of breaks must be equal to the number of labels specified by "leg_labels".
#'   If this is not TRUE, `leg_breaks` and `leg_labels` are reset to `waiver()`
#'   (TBD)  Default: NULL (the default ggplot2 `labels = waiver()` is used)
#' @param leg_colors `characrter (n_leg_labels)` Colors to be assigned to
#'   the different values of `fill_var` if `palette_name` == "manual". The number
#'   of colors must be equal to the number of unique values of `fill_var`, otherwise
#'   an error will be issued. Colors can be specified as `R` color names (e.g.,
#'   `leg_colours = c("red", "blue")`, HEX values (e.g., `leg_colours = c(#8F2525,
#'    #41AB96)`, or a mix of the two. Note that the argument is __mandatory__ if
#'    `palette_name` == "manual", and __ignored__ on all other palettes,
#'    Default: NULL
#' @param leg_position `character ["right" | "bottom"]` Specifies if plotting
#'   the legend on the right or on the bottom. Default: "right"
#' @param maxpixels `numeric` maximum number of pixels to be used for plotting *for
#'   each band*. Reduce this to speed-up plotting by subsampling the raster (this
#'   reduces quality!). Increase it to improve quality (this reduces rendering
#'   speed!), Default:1e5
#' @param band_names `character (nbands)`, array of band names. These will used
#'   to populate the "strips" above each plotted band in case of multi-band plot.
#'   If NULL, bnames are retrieved from the input raster using `sprawl::get_rastinfo`,
#'   Default: NULL
#' @param bands_to_plot `numeric array`, array of band numbers to be plotted in
#'   case `in_rast` is multi-band. If NULL, all bands are plotted separately
#'   using facet_wrap, Default: NULL
#' @param facet_rows `numeric`, number of rows used for plotting multiple bands,
#'   in faceted plots, Default: 2
#' @param xlims `numeric(2)`, minimum and maximum x coordinates to be plotted.
#'   If NULL, the whole x-range is plotted, Default: NULL
#' @param ylims `numeric(2)`, minimum and maximum y coordinates to be plotted.
#'   If NULL, the whole y-range is plotted, Default: NULL
#' @param zlims `numeric array [2]` limits governing the range of
#'  values to be plotted (e.g., c(0.2,0.4)), Default: NULL
#' @param zlims_type `character ["vals" | "percs"]` type of zlims specified.
#'   - "vals": zlims indicates the range of values to be plotted
#'   - "percs": zlims indicates the range of percentiles to be plotted (e.g.,
#'      specifying zlims = c(0.02, 0.98), zlim_type = "percs" will plot the
#'      values between the 2nd and 98th percentile). Ignored if `zlims` is not
#'      set, Default: "vals"
#' @param outliers_style `character ["censor" | "to_minmax"]` specifies how
#'   the values outside of the zlims range will be plotted.
#'   - If == "censor", they are plotted using the colors(s) specified in `outliers_color`
#'   - If == "to_minmax", outliers are forced to the colors used for the maximum
#'     and minimum values specified in `zlims` (using `scales::squish`), Default:
#'     `censor`.
#' @param outliers_colors `character array (length 1 or 2)` specifies colors to be
#'   used to plot values outside zlims if `outliers_style == "censor".
#'   If only one color is passed, both values above max(zlims) and below min(zlims)
#'   are plotted with the same color. If two colors are passed, the first color
#'   is used to plot values < min(zlims) and the second to plot colors > max(zlims),
#'   Default: c("grey10", "grey90")
#' @param basemap `character` If not NULL and valid, the selected basemap is
#'   used as background. For a list of valid basemaps, see `rosm::osm.types()`,
#'   Default: NULL
#' @param zoomin `numeric`, Adjustment factor for basemap zoom. Negative values
#'   lead to less detailed basemap, but larger text. Default: 0
#' @param borders_layer `character` object of class `sf_POLYGON` or `sfc_polygon`,
#'   (or coercible to it using `sprawl::cast_vect`) to be overlayed to the plot,
#'   Default: NULL (no overlay)
#' @param borders_color color used to plot the boundaries of `borders_layer`
#'   (if provided), Default: 'grey15'
#' @param borders_size size used to plot the boundaries lines of `borders_layer`
#'   (if provided), Default: 0.2
#' @param borders_txt_field name of the column of `borders_layer` to be used to
#'   add text labels to `borders_layer` (if provided), Default: NULL (no labels)
#' @param borders_txt_size size of the txt labels derived from `borders_layer`,
#'   Default: 2
#' @param borders_txt_color color of the txt labels derived from `borders_layer`,
#'   Default: "grey15"
#' @param scalebar `logical` If TRUE, add a scalebar on the bottom right corner,
#'   Default: TRUE
#' @param scalebar_dist `numeric` Width of the scale bar (in km). If NULL, it
#'   is computed automatically on the basis of the range in x direction,
#'   Default: NULL
#' @param scalebar_txt_dist `numeric` Distance between scalebar and its labels.
#'   Adjust this in case of overlap, Default: 0.30
#' @param transparency `numeric [0,1]`, transparency of the raster layer. Higher
#'   values lead to higher transparency, Default: 0 (ignored if basemap == NULL)
#' @param na.color `character`, color to be used to plot NA values,
#'   Default: 'transparent'.
#' @param na.value `numeric`, Additional values to be treated as NA, Default: NULL
#' @param show_axis `logical`, If FALSE, axis names and labels are suppressed,
#'   Default: TRUE
#' @param show_grid `logical`, If FALSE, graticule lines are suppressed,
#'   Default: TRUE
#' @param grid_color `character` color to be used to plot grid lines,
#'  Default: grey15"
#' @param title `character`, Title of the plot, Default: "Raster Plot"
#' @param subtitle Subtitle of the plot, Default: NULL
#' @param theme `theme function` ggplot theme to be used
#' (e.g., theme_light()), Default: theme_bw()
#' @param verbose `logical`, If FALSE, suppress processing message,
#'  Default: TRUE
#' @return a `gg` plot. It is plotted immediately. If the call includes
#'  an assignment operator (e.g., `plot <- plot_rast_gg(in_rast))`, the plot is
#'  saved to the specified variable. Otherwise, it is plotted immediately.
#' @examples
#' \dontrun{
#' #single band plot
#'  in_rast <- read_rast(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm",
#'                   na.value = 0, na.color = "transparent",
#'                   title = "OLI", subtitle = "Band 2")
#'
#'  #Change basemap and transparency
#'  plot_rast_gg(in_rast, basemap = "stamenbw",
#'                   palette_name = "RdYlBu",
#'                   show_axis = T,
#'                   na.value = 0, na.color = "transparent",
#'                   transparency = 0.2,
#'                   title = "OLI - 15/06/2017",
#'                   subtitle = "Band 2 - Green")
#'
#' #Multi band plot with selection of bands
#'  in_rast <- read_rast(system.file("extdata/OLI_test",
#'   "oli_multi_1000.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm", bands_to_plot = c(1,4),
#'                   na.value = 0, na.color = "transparent",
#'                   title = "OLI", subtitle = "Band 2 vs band 4",
#'                   facet_row = 1)
#'
#' #Adjusting limits and changing palette
#'  in_rast <- raster::stack(system.file("extdata/REYE_test",
#'                           "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data"))
#'  #vanilla, with borders added
#'  borders <- get_boundaries("ITA", 3)
#'  plot_rast_gg(in_rast,
#'              palette_name = "RdYlBu",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
#'              borders_layer = borders, borders_txt_field = "NAME_3")
#'
#'  # limits adjusted on the 10th and 99.9th percentile to remove very low
#'  # values outliers and better use the palette on high values (note that
#'  # outliers can be set as transparent!) + increase maxpixels to plot at
#'  # full resolution.
#'  plot_rast_gg(in_rast, basemap = "osm", maxpixels = 500e5,
#'              palette_name = "RdYlBu",
#'              zlims = c(0.10, 0.999), zlims_type = "percs",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
#'              outliers_style = "censor",
#'              outliers_colors = c("transparent", "purple"))
#'
#'  # Adjust `maxpixels` to speed-up rendering by sacrificing quality
#'  plot_rast_gg(in_rast, basemap = "osm", maxpixels = 10e4,
#'              palette_name = "RdYlBu",
#'              zlims = c(0.10, 0.999), zlims_type = "percs",
#'              title = "gNDVI - 14/03/2016", subtitle = "From RapidEye",
#'              outliers_style = "to_minmax",
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
#'  coord_fixed ggplot
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
  rast_type    = NULL,
  palette_name = NULL, direction = 1,
  leg_type     = NULL, leg_labels = NULL, leg_colors = NULL,
  leg_breaks   = NULL, leg_position = "right",
  maxpixels    = 1e5,
  band_names   = NULL, bands_to_plot = NULL, facet_rows = 2,
  xlims        = NULL, ylims = NULL,
  zlims        = NULL, zlims_type = "vals",
  outliers_style = "censor", outliers_colors = c("grey10", "grey90"),
  basemap      = NULL, zoomin = 0,
  borders_layer = NULL, borders_color = "grey15", borders_size = 0.2,
  borders_txt_field = NULL, borders_txt_size = 3, borders_txt_color = "grey15",
  scalebar     = TRUE, scalebar_dist = NULL, scalebar_txt_dist = 0.03,
  transparency = 0,
  na.color     = NULL, na.value = NULL,
  show_axis    = TRUE, show_grid = TRUE, grid_color = "grey20",
  title = NULL, subtitle = NULL,
  theme        = theme_bw(),
  verbose      = TRUE
) {

  geometry <- NULL
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

  if (!is.null(basemap)) {
    assertthat::assert_that(
      basemap %in% rosm::osm.types(),
      msg = strwrap(
        "plot_rast_gg --> Invalid `basemap`. See `rosm::osm.types()` for
        valid value. Aborting!")
    )
  }
  # -----------------------------------------------------------
  # Set default palettes for different categories and get info
  # on valid pals

  if (is.null(rast_type)) {
    if (length(unique(raster::sampleRegular(in_rast, size = 500000))) > 25) {
      palette_type = "cont"
    } else {
      palette_type = "qual"
    }
  } else {
    palette_type <- ifelse(rast_type == "continuous", "cont", "qual")
  }
  def_palettes = list(qual = "hue",
                      cont  = "Greys")
  def_legtypes = list(qual = "legend",
                      cont  = "colourbar")

  fill_pals   <- fillpals()
  valid_pals  <- fill_pals[fill_pals$cont_qual == palette_type,]

  # -----------------------------------------------------------
  # Check validity of palette name.
  # reset to default if palette not valid for selected palette_type
  if (!is.null(palette_name)) {

    if (!palette_name %in% valid_pals$name) {
      warning("plot_vect --> The selected palette name is not valid.\n",
              "Reverting to default value for the variable of interest (",
              as.character(def_palettes[palette_type]), ")")
      palette_name <- as.character(def_palettes[palette_type])
    }
  } else {
    palette_name <-   as.character(def_palettes[palette_type])
  }

  palette <- valid_pals[which(valid_pals$name == palette_name),]
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
                       "discrete"   = "legend",
                       "continuous" = "colourbar"
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
  #   (May need to save a temporary file to disk if the raster is only in
  #   memory. Since in_memory rasters are always small, we can afford it!)

  rastinfo <- get_rastinfo(in_rast, verbose = FALSE)
  if (!is.null(basemap)) {
    if (any(rastinfo$fnames == "")) {
      rastfile <- cast_rast(in_rast, "rastfile")
      in_rast  <- read_rast(rastfile)
    }
    rastinfo$fnames <- get_rastinfo(in_rast, verbose = FALSE)$fnames


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

  in_rast_fort <- ggplot2::fortify(in_rast, format = "long") %>%
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

    all_lims <- in_rast_fort[, as.list(stats::quantile(value, zlims,
                                                       na.rm = TRUE)),
                             by = band]
    zlims <- c(min(all_lims[,2]), max(all_lims[,3]))

  }

  if (!is.null(zlims)) {
    #   ____________________________________________________________________________
    #   If oob_style == "censor" create additional data tables                 ####
    #   containing only values above / below limits
    if (outliers_style == "censor") {

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

  # Blank plot
  plot <- ggplot() + theme +
    theme(panel.grid.major = element_line(color = grid_color, linetype = 3),
          panel.grid.minor = element_blank())
  ggtitle(title,
          subtitle = subtitle)

  # Center the title - can be overriden in case after plot completion
  plot <- plot +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "transparent"))


  #   __________________________________________________________________________
  #   add background map - need to do this here to prevent "shadowing"      ####

  if (!is.null(basemap)) {

    osm_plot <- suppressMessages(
      ggspatial::geom_osm(zoomin = zoomin, type   = basemap, progress = "none")
    )
    plot <- plot + osm_plot
  }

  #   __________________________________________________________________________
  #   add the raster layer                                                  ####

  plot <- plot +
    geom_raster(data = in_rast_fort, aes(x, y, fill = value),
                alpha = 1 - transparency, na.rm = TRUE ,
                hjust = 1, vjust = 1 )
  if (rastinfo$nbands > 1) {
    plot <- plot + facet_wrap(~band, nrow = facet_rows, drop = T)
  }

  # Check if the raster has an attribute table. If so, use the class names
  # to fill-in leg_labels automatically if they were not provided by the user.

  if (is.null(leg_labels)) {
    if (length(in_rast@data@attributes) != 0 ) {
      leg_labels <- in_rast@data@attributes[[1]]$Class
    }

  }

  #   __________________________________________________________________________
  #   Modify the palette according to variable type and palette             ####

  plot <- add_scale_fill(plot,
                         palette,
                         title = "Value",
                         na.color,
                         zlims,
                         leg_breaks,
                         leg_labels,
                         leg_colors,
                         leg_type,
                         outliers_style,
                         direction)
  plot <- plot + theme(legend.justification = "center",
                       legend.box.spacing = grid::unit(10,"points"))

  #   __________________________________________________________________________
  #   put legend on bottom if requested                                     ####
  if (leg_position == "bottom") {
    plot <- plot + theme(legend.position = "bottom")
    if (palette_type == "qual" | leg_type == "legend") {
      plot <- plot + guides(fill = guide_legend(title.position = "bottom",
                                                title.hjust = 0.5))
    } else {

      plot <- plot + guides(
        fill = guide_colourbar(title.position = "bottom",
                               title.hjust = 0.5,
                               barwidth = grid::unit(0.25 , "npc")))
    }
  }

  #   __________________________________________________________________________
  #   if oob_style = censor, add "layers" corresponding to oob data          ####

  if (outliers_style == "censor" & !is.null(zlims)) {

    if (length(unique(outliers_colors)) == 1) {

      dummy_data <- data.frame(out_high_tbl[1,]) %>%
        dplyr::mutate(cat = c("Out"),
                      color = c(out_high_color),
                      band = levels(in_rast_fort$band)[1])
      plot <- plot + geom_polygon(data = dummy_data,
                                  aes(x = x,
                                      y = y,
                                      colour = color))
      plot <- plot + scale_colour_manual(
        "Outliers",
        values = out_high_color,
        labels = paste0("< ", format(zlims[1], digits = 2)
                        , " OR ", "> ", format(zlims[2], digits = 2))
      )

      plot <- plot +
        guides(colour = guide_legend(
          title = "Outliers", override.aes = list(fill = out_high_color,
                                                  color = "black")))
    } else {

      dummy_data <- data.frame(rbind(out_high_tbl[2,], out_low_tbl[1,])) %>%
        dplyr::mutate(cat = c("High","Low"),
                      color = c(out_low_color, out_high_color),
                      band = levels(in_rast_fort$band)[1])
      plot <- plot + geom_polygon(data = dummy_data,
                                  aes(x = x, y = y,
                                      colour = color))
      plot <- plot + scale_color_manual(
        "Outliers",
        values = c(out_low_color, out_high_color),
        labels = c(paste0("< ", format(zlims[1], digits = 2)),
                   paste0("> ", format(zlims[2], digits = 2)))
      )
      plot <- plot +
        guides(colour = guide_legend(
          title = "Outliers",
          override.aes = list(fill = c(out_low_color, out_high_color),
                              color = "black")))
    }

    plot <- plot + geom_raster(data = out_high_tbl,
                               aes(x = x, y = y), hjust = 1 , vjust = 1,
                               fill = out_high_color, na.rm = TRUE)
    plot <- plot + geom_raster(data = out_low_tbl,
                               aes(x = x, y = y), hjust = 1 , vjust = 1,
                               fill = out_low_color,
                               na.rm = TRUE)

  }

  #   __________________________________________________________________________
  ##  If borders passed, add a "borders" layer                              ####

  if (!is.null(borders_layer)) {
    if (is.null(basemap)) {

      borders_layer <- sf::st_transform(borders_layer,
                                        get_proj4string(in_rast)) %>%
        crop_vect(in_rast)
      plot <- plot + geom_sf(data  = borders_layer,
                             fill  = "transparent",
                             color = borders_color,
                             size  = borders_size) +
        coord_sf(crs = get_proj4string(in_rast),
                 # expand = FALSE,
                 xlim = xlims,
                 ylim = ylims)
      if (!is.null(borders_txt_field)) {
        if (borders_txt_field %in% names(borders_layer)) {
          borders_layer <- borders_layer %>%
            dplyr::mutate(
              lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
              lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])) %>%
            sf::st_as_sf()
          plot <- plot + geom_text(data = borders_layer,
                                   aes_string(label = borders_txt_field,
                                              x = "lon", y = "lat"),
                                   size = borders_txt_size,
                                   color = borders_txt_color)
        }
      }
    } else {
      warning("plot_rast_gg --> You can not currently use both a `basemap`" ,
              "and a `borders_layer`. The `borders_layer` will be ignored.",
              "This will be fixed in future versions.")
    }
  }

  # set axes names
  if (rastinfo$units == "dec.degrees") {
    xlab = "Longitude"
    ylab = "Latitude"
  } else {
    xlab = paste0("Easting [",rastinfo$units,"]")
    ylab = paste0("Northing [",rastinfo$units,"]")
  }

  #   _________________________________________________________________________
  #   Add scalebar                                                          ####

  if (scalebar) {

    plot <- plot +
      sprawl_scalebar(in_rast,
                      scalebar_dist  = scalebar_dist,
                      x.min = xlims[1], x.max = xlims[2],
                      y.min = ylims[1], y.max = ylims[2],
                      location = "bottomright", st.size = 3.5,
                      st.bottom = FALSE, model = NULL,
                      st.dist = scalebar_txt_dist, units = rastinfo$units)
  }

  #   __________________________________________________________________________
  #   Finalize the plot and return it                                       ####
  #   - currently, different options depending on wheter "basemap" was
  #     requested, due to impossibility of using coord_sf alongside ggosm.
  #
  plot <- plot +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
          panel.ontop = T,
          panel.background = element_rect(fill = "transparent"))

  if (is.null(basemap)) {
    plot <- plot  +
      coord_sf(crs = get_proj4string(in_rast), ndiscr = 1000,
               expand = FALSE            ) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  } else {
    plot <- plot +
      coord_fixed() +
      scale_x_continuous(xlab,
                         # expand = expand_scale(mult = 0.0005),
                         limits = c(xlims[1], xlims[2])) +
      scale_y_continuous(ylab,
                         # expand = expand_scale(mult = 0.0005),
                         limits = c(ylims[1], ylims[2]))


  }
  # Remove graticule if show_grid == TRUE
  if (!show_grid) {
    plot <- plot + theme(panel.grid.major = element_line(colour = 'transparent'),
                         panel.grid.minor = element_line(colour = 'transparent'))
  }

  # Remove axes if show_axis == TRUE
  if (!show_axis) {

    plot <- plot  +
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
  }
  plot
}
