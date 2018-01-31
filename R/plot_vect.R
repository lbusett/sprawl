#' @title plot a map based on a `vector` object using `ggplot::geom_sf``
#' @description Plot a raster object using ggplot with an (optional) basemap.
#'   The function allows to:
#'   - Plot either only the geometry of `in_vect`, or a map with different
#'     fill colors based on a column of `in_vect`;
#'   - Plot a single- or multi-band image using facet_wrap (args `facet_var`,
#'    `facet_rows`);
#'   - "Easily" select different palettes (arg `palette_name`) for the fill
#'     variable;
#'   - Easily control plotting limits on the z-dimension, specifying either
#'     values or quantiles ranges (args `zlims`, `zlims_type`), as well as how
#'     the outlier values are represented (args `outliers_style` and
#'     `outliers_color`)
#'   - Easily control breaks and labels in legends (args `leg_type`, `leg_breaks`,
#'     `leg_labels`, `leg_position`)
#'   - Automatically add a scalebar to the plot (args`scalebar`, `scalebar_dist`,
#'     `scalebar_txt_dist`, `scalebar_txt_size`)
#'   - Easily add an additional "vector" layer (e.g., administrative boundaries)
#'     (args `borders_layer`, `borders_color`, `borders_....`)
#' See the description of the arguments for details on their use
#' @param in_vect object of class `sfc_POLYGON` (or coercible to it using
#'  `sprawl::cast_vect)
#' @param line_color `character` color used to plot the polygon borders of
#'  `in_vect`
#' @param line_size `numeric` size of lines used to plot the polygons borders of
#'  in_vect, Default: 0.2
#' @param point_shape `character ["circle" | "square" | "diamond" | "uptriangle"
#'  | "downtriangle"]` symbol used to plot points if in_vect is a "POINT" or
#'  "MULTIPOINT" object, default: `circle`
#' @param point_size `numeric` size of the symbol used to plot points if in_vect
#'  is a "POINT" or "MULTIPOINT" object, Default = 0.2
#' @param point_linecolor `character` color of the border of the symbol used to plot
#'  points if in_vect is a "POINT" or "MULTIPOINT" object
#' @param point_linesize `numeric` size of the symbol used to plot points if in_vect is
#'   a "POINT" or "MULTIPOINT" object, Default: 0.2
#' @param fill_var `character` name of the column of `in_vect` to be used for
#'  coloring the different polygons. If NULL, only the geometry is plotted,
#'  Default: NULL
#' @param txt_field name of the column of `borders_layer` to be used to
#'   add text labels to `borders_layer` (if provided), Default: NULL (no labels)
#' @param txt_size size of the txt labels derived from `borders_layer`,
#'   Default: 2
#' @param txt_color color of the txt labels derived from `borders_layer`,
#'   Default: "grey15"
#' @param scale_transform `character` optional transformation to be applied on
#'  values of continuous fill variables (e.g., "log"),  Default: NULL
#' @param transparency `numeric [0, 1]`transparency of the filled polygons/points. Higher
#'   values lead to higher transparency, Default: 0 (ignored if fill_var == NULL)
#' @param facet_var `character` name of a column of `in_vect` to be used for
#'  making different facets of the plot. Useful in case multiple values of `fill_var`
#'  are available for each polygon, according to a grouping variable.
#'  If NULL, multiple values of `fill_var` for the same polygon are ignored
#'  (the first value found is used), Default: NULL (see examples)
#' @param facet_rows `numeric`, number of rows used for plotting multiple bands,
#'   in faceted plots, Default: 2 (Ignored if `fill_var` == NULL)
#' @param levels_to_plot `character array` If `facet_var` is specified, allows
#'   to select which levels of the vaiable should be plotted. If NULL, all the
#'   levels availbale will be plotted, Default: NULL
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
#' @param basemap `character` If not NULL and valid, the selected basemap is
#'   used as background. For a list of valid basemaps, see `rosm::osm.types()`,
#'   Default: NULL (Currently not yet supported!)
#' @param extent
#'   1.`numeric (4)` xmin, ymin. xmax. ymax of the desired area, **in WGS84 lat/lon**
#'      **coordinates** OR
#'   2. object of class `sprawl_ext` OR
#'   3. any object from which an extent can be retrieved using `sprawl::get_extent()`.
#'
#'  If NULL, plotting extent is retrieved from `in_rast`, Default: NULL
#' @param zoomin `numeric`, Adjustment factor for basemap zoom. Negative values
#'   lead to less detailed basemap, but larger text. Default: 0 (Currently not
#'   yet supported!)
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
#' @param scalebar `logical` If TRUE, add a scalebar on the bottom right corner,
#'   Default: TRUE
#' @param scalebar_dist `numeric` Width of the scale bar (in km). If NULL, it
#'   is computed automatically on the basis of the range in x direction,
#'   Default: NULL
#' @param scalebar_pos `character ["bottomright" | "bottomleft" | "topleft" | "topright"]`
#'  indicating where the scalebar should be placed, Default: "bottomright"
#' @param scalebar_txt_dist `numeric` Distance between scalebar and its labels.
#'   Adjust this in case of overlap, Default: 0.30
#' @param na.color `character`, color to be used to plot NA values,
#'   Default: 'transparent'.
#' @param na.value `numeric`, Additional values of `fill_var` to be treated as
#'  NA, Default: NULL
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
#' @param leg_colors `character (n_leg_labels)` Colors to be assigned to
#'   the different values of `fill_var` if `palette_name` == "manual". The number
#'   of colors must be equal to the number of unique values of `fill_var`, otherwise
#'   an error will be issued. Colors can be specified as `R` color names (e.g.,
#'   `leg_colors = c("red", "blue")`, HEX values (e.g., `leg_colours = c(#8F2525,
#'    #41AB96)`, or a mix of the two. Note that the argument is __mandatory__ if
#'    `palette_name` == "manual", and __ignored__ on all other palettes,
#'    Default: NULL
#' @param leg_position `character ["right" | "bottom"]` Specifies if plotting
#'   the legend on the right or on the bottom. Default: "right"
#' @param show_axis `logical`, If FALSE, axis names and labels are suppressed,
#'   Default: TRUE
#' @param show_grid `logical`, If FALSE, graticule lines are suppressed,
#'   Default: TRUE
#' @param grid_color `character` color to be used to plot grid lines,
#'  Default: grey15"
#' @param title `character`, Title of the plot, Default: "Vector Plot"
#' @param subtitle Subtitle of the plot, Default: NULL
#' @param theme `theme function` ggplot theme to be used
#' (e.g., theme_light()), Default: theme_bw()
#' @param verbose `logical`, If FALSE, suppress processing message,
#'  Default: TRUE
#' @return a `gg` plot. It is plotted immediately. If the call includes
#'  an assignment operator (e.g., `plot <- plot_vect(in_vect))`, the plot is
#'  saved to the specified variable. Otherwise, it is plotted immediately.
#' @details DETAILS
#' @examples
#'
#'  library(ggplot2)
#'  in_vect <- get(load(system.file("extdata/shapes", "poly_lomb.RData",
#'                                  package = "sprawl.data")))
#'  # plot only geometry
#'  plot_vect(in_vect)
#'
#'  plot_vect(in_vect, line_color = "blue", line_size = 1.5)
#'
#'  # plot with a fill on a cartegorical variable with a single "level"
#'  plot_vect(in_vect, fill_var = "NAME_2")
#'
#'  # change the palette, add a scalebar and remove the grid
#'  plot_vect(in_vect, fill_var = "NAME_2",show_axis = FALSE, palette_name = "Set3",
#'            scalebar = TRUE, grid = FALSE)
#'
#'
#'  # plot with a fill on a continuous variable with two "levels", using facets
#'  # and a diverging palette. also add a "borders" layer with a different
#'  #plot color
#'  plot_vect(in_vect, fill_var = "population", facet_var = "year",
#'            palette = "RdYlBu", scalebar = TRUE, scalebar_dist = 50,
#'            grid = FALSE, zlims = c(5,20), outliers_colors = c("yellow", "green"),
#'            borders_layer = get_boundaries("ITA", level = 2),
#'            borders_color = "red", borders_txt_field = "NAME_2")
#'
#'
#'  # plot a "points" layer
#'  library(sp)
#'  demo(meuse, ask = FALSE, echo = FALSE)
#'  plot_vect(meuse, scalebar_dist = 0.5)
#'  plot_vect(meuse, fill_var = "copper",
#'                   point_shape = "diamond",
#'                   palette_name = "RdYlGn",
#'                   scalebar_dist = 0.5)
#'
#' @rdname plot_vect
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom rosm osm.types
#' @importFrom sf st_transform
#' @importFrom grid unit
#' @importFrom methods is
#' @importFrom stats as.formula
#' @importFrom purrr map_dbl
#' @importFrom ggplot2 theme_bw fortify ggplot scale_x_continuous expand_scale
#'  scale_y_continuous ggtitle theme element_blank element_text element_rect
#'  geom_raster aes facet_wrap scale_fill_brewer scale_fill_distiller waiver
#'  geom_polygon scale_colour_manual guides guide_legend scale_color_manual
#'  coord_fixed geom_sf aes_string coord_sf margin guide_colourbar element_line
plot_vect <- function(
  in_vect,
  line_color     = "black", line_size     = 0.2,
  point_size = 0.2, point_shape = "circle",
  point_linecolor = "black", point_linesize = 0.01,
  fill_var       = NULL,
  txt_field = NULL, txt_size = 2.5, txt_color = "grey15", #nolint
  scale_transform= NULL, transparency = 0,
  facet_var      = NULL, facet_rows     = NULL,
  levels_to_plot = NULL,
  borders_layer  = NULL, borders_color = "grey15", borders_size = 0.2,
  borders_txt_field = NULL, borders_txt_size = 2.5, borders_txt_color = "grey15", #nolint
  basemap        = NULL, zoomin = 0,
  extent         = NULL,
  zlims          = NULL, zlims_type = "vals",
  outliers_style = "censor", outliers_colors = c("grey10", "grey90"),
  scalebar       = TRUE, scalebar_dist = NULL, scalebar_txt_dist = 0.03,
  scalebar_pos   = "bottomright",
  na.color       = NULL,  na.value = NULL,
  palette_name   = NULL,  direction = 1,
  leg_type       = NULL, leg_labels = NULL, leg_breaks = NULL,
  leg_colors     = NULL, leg_position = "right",
  show_axis      = TRUE, show_grid = TRUE, grid_color = "grey15",
  title          = NULL, subtitle = NULL,
  theme          = theme_bw(),
  verbose        = TRUE
) {

  call = match.call()
  color <- NULL

  in_vect <- cast_vect(in_vect, "sfobject")
  assertthat::assert_that(
    methods::is(in_vect, "sf"),
    msg = "plot_vect --> `in_vect` is not a valid `sf` object. Aborting!"
  )

  geocol <- attr(in_vect, "sf_column")

  if (!is.null(facet_var)) {
    if (!any(names(in_vect) == facet_var)) {
      warning("The Specified `facet_var` is not a column of ",
              call[2], ". It will be ignored")
    }
    if (!class(in_vect[[facet_var]]) %in% c("character", "factor")) {
      warning("The Specified `facet_var` is not of class `character` or",
              "`factor`. It will be ignored.")
      facet_var <- NULL
    }
    if (!is.null(levels_to_plot)) {

      in_vect <- in_vect %>%
        dplyr::filter_(fill_var %in% levels_to_plot) %>%

        if (length(in_vect) == 0 ) {
          stop("Specified values of ", call$levels_to_plot, " do note exist in ",
               call$in_vect, ". Please check your inputs!")
        }
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
    warning("`fill_var` not specified. Only the geometry will be plotted!")
    fill_var     <- names(in_vect)[1]
    na.color     <- "transparent"
    palette_type <- "qual"
    no_fill <- TRUE
  } else {

    # check the class of fill_var. If it is a factor or a character, palette_type
    # is set to "qual", otherwise to "cont".
    cls_fill_var <- class(in_vect[[fill_var]])
    if (cls_fill_var %in% c("factor", "character")) {
      palette_type <- "qual"
    } else {
      palette_type <- "cont"
    }
    no_fill <- FALSE
  }

  # -----------------------------------------------------------
  # Set default palettes for different categories and get info
  # on valid pals

  def_palettes = list(qual = "hue",
                      cont  = "Greys")
  def_legtypes = list(qual = "legend",
                      cont  = "colourbar")
  fill_pals   <- fillpals()
  valid_pals  <- fill_pals[fill_pals$cont_qual == palette_type,]


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

  if (length(outliers_colors) == 1) {
    out_high_color <- out_low_color <- outliers_colors
  } else {
    out_high_color = outliers_colors[2]
    out_low_color  = outliers_colors[1]
  }

  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  # if (!is.null(basemap)) {
  #   if (verbose) {
  #     message("plot_rast_gg --> Reprojecting the input raster to epsg:3857")
  #   }
  #   in_vect <- sf::st_transform(in_vect, "+init=epsg:3857")
  # }

  #   __________________________________________________________________________
  #   set x/y limits                                                        ####
  #   Set the plotting extent ---
  #   __________________________________________________________________________
  #   If no geo limits passed, compute them from the input raster           ####
  #   otherwise try to use the provided extent

  if (is.null(extent)) {
    plot_ext <- get_extent(in_vect)
  } else {
    if (inherits(extent, "numeric") & length(extent) == 4) {

      names(extent) <- c("xmin", "ymin", "xmax", "ymax")
      plot_ext <- methods::new("sprawlext",
                               extent     = extent,
                               proj4string = get_proj4string(4326))
      plot_ext <-  reproj_extent(plot_ext, get_proj4string(in_vect))
    } else {

      plot_ext <- get_extent(extent, abort = FALSE)
      if (inherits(plot_ext, "sprawlext")) {
        plot_ext <-  reproj_extent(plot_ext, get_proj4string(in_vect))
      } else {
        message("plot_rast_gg --> Impossible to retriebve an extent from `",
                call$extent,
                "`\n Resetting the extent to that of `", call[[2]], "`")
        plot_ext <- get_extent(in_vect)

      }
    }
  }
  xlims <- plot_ext@extent[c(1,3)]
  ylims <- plot_ext@extent[c(2,4)]
  #   _________________________________________________________________________
  #   if additional NA value passed, convert corresponding values of
  #   fill_var column to NA ####
  if (!is.null(na.value)) {
    in_vect[which(in_vect[fill_var] == na.value),][fill_var] <- NA
  }

  #   _________________________________________________________________________
  #   if transparent NA, we can remove the NAs from the data to speed-up   ####
  #   rendering
  if (!is.null(na.color)) {
    if (na.color == "transparent")  {
      in_vect <- in_vect[!is.na(in_vect[[fill_var]]),]
    }
  }

  #   _________________________________________________________________________
  #   if zlims is of type "percs" compute the reqauired quantiles   ####

  if (!is.null(zlims) & zlims_type == "percs") {

    all_lims <- stats::quantile(in_vect[[fill_var]], zlims, na.rm = TRUE)
    zlims <- c(min(all_lims[]), max(all_lims[2]))

  }

  # On continuous variables, perform processing to create the legends for
  # outliers "right"
  if (palette_type != "qual") {
    if (!is.null(zlims)) {

      #   ____________________________________________________________________________
      #   If outliers_style == "censor" create additional data tables                 ####
      #   containing only values above / below limits
      if (outliers_style == "censor") {

        out_low_tbl  <- in_vect[which(in_vect[[fill_var]] < zlims[1]),]
        out_high_tbl <- in_vect[which(in_vect[[fill_var]] > zlims[2]),]

        if (out_high_color == "transparent") {
          in_vect <- in_vect[which(in_vect[[fill_var]] < zlims[2]),]
        }

        if (out_low_color == "transparent") {
          in_vect <- in_vect[which(in_vect[[fill_var]] > zlims[1]),]
        }

      }
    }
  }

  # Blank plot
  plot <- ggplot() +
    theme +
    theme(panel.grid.major = element_line(color = grid_color, linetype = 3),
          panel.grid.minor = element_blank()) +
    coord_sf(crs = get_proj4string(in_vect), ndiscr = 1000,
             expand = FALSE,
             xlim = c(xlims[1], xlims[2]),
             ylim = c(ylims[1], ylims[2])) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    ggtitle(title, subtitle = subtitle)

  #   __________________________________________________________________________
  #   add background map - need to do this here to prevent "shadowing"      ####
  #
  #   if (!is.null(basemap)) {
  #
  #     osm_plot <- suppressMessages(
  #       ggspatial::geom_osm(zoomin = zoomin, type = basemap, progress = "text")
  #     )
  #     vect <- fortify(as(in_vect, "Spatial"))
  #     names(vect)[1:2] <- (c("x","y"))
  #     plot + osm_plot + geom_spatial(as(in_vect, "Spatial"), aes(x = long, y = lat, fill = NAME_2))
  #
  #
  #     bmap <- get_map(sp::bbox(
  #       as(sf::st_transform(as(get_extent(in_vect), "sfc_POLYGON"), 4326), "Spatial")
  #       ), maptype = "roadmap")
  #     plot <- plot + ggmap(bmap)
  #   }

  #   __________________________________________________________________________
  #   Add the fill                                                          ####
  if (no_fill) {
    plot <- plot + geom_sf(data = in_vect,
                           fill  = "transparent",
                           color = line_color,
                           size  = line_size)
  } else {

    if (inherits(in_vect[[geocol]], c("sfc_POINT", "sfc_MULTIPOINT"))) {

      point_shapes <- data.frame(shape = c("circle", "square", "diamond",
                                           "uptriangle", "downtriangle"),
                                 number = c(21,22,23,24,25))

      point_code <- point_shapes[which(point_shapes$shape == point_shape),]$number

      plot <- plot + geom_sf(data = in_vect,
                             aes_string(fill = fill_var),
                             shape = point_code,
                             colour = point_linecolor,
                             size  = point_size,
                             alpha = 1 - transparency) +
        coord_sf(xlim = xlims, ylim = ylims)

    } else {

      plot <- plot + geom_sf(data = in_vect,
                             aes_string(fill = fill_var),
                             size  = line_size,
                             color = line_color,
                             alpha = 1 - transparency) +
        coord_sf(xlim = xlims, ylim = ylims)
    }
    plot <- add_scale_fill(plot,
                           palette,
                           scale_transform,
                           title = fill_var,
                           na.color,
                           zlims,
                           leg_breaks,
                           leg_labels,
                           leg_colors,
                           leg_type,
                           outliers_style,
                           direction)


    #   __________________________________________________________________________
    #   Modify the palette according to variable type and palette             ####

    plot <- plot + theme(legend.justification = "center",
                         legend.box.spacing = grid::unit(10,"points"))
  }


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
                               barwidth = grid::unit(0.5 , "npc")))
    }
  }

  #   _________________________________________________________________________
  #   Facet the plot if a proper faceting variable was passed               ####

  if (!is.null(facet_var)) {
    in_vect[[facet_var]] <- as.factor(in_vect[[facet_var]])
    if (is.null(facet_rows)) {
      facet_rows <- floor(length(unique(in_vect[[facet_var]])) / 2)
    }
    plot <- plot + facet_wrap(as.formula(paste("~", facet_var)),
                              nrow = facet_rows, drop = TRUE)
  }


  #   ____________________________________________________________________________
  #   if oob_style = censor, add "layers" corresponding to oob data          ####

  if (outliers_style == "censor" & !is.null(zlims)) {

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
                           na.rm = TRUE
    )

    plot <- plot + geom_sf(data  = out_low_tbl,
                           fill  = out_low_color,
                           na.rm = TRUE)

  }

  if (!is.null(txt_field)) {
    if (txt_field %in% names(in_vect)) {

      centroids <- st_centroid(in_vect)
      centroids <-  do.call(rbind, st_geometry(centroids)) %>%
        as_tibble() %>% setNames(c("lon","lat"))
      lab_layer <- in_vect %>%
        mutate(lon = centroids$lon, lat = centroids$lat)
      plot <- plot + geom_text(data = lab_layer,
                               aes_string(label = txt_field,
                                          x = "lon", y = "lat"),
                               size = txt_size,
                               color = txt_color)
    }
  }

  #   __________________________________________________________________________
  ##  If borders passed, add a "borders" layer                              ####

  if (!is.null(borders_layer)) {
    borders <- sf::st_transform(borders_layer, get_proj4string(in_vect))
    # %>%
    #   crop_vect(in_vect)
    plot    <- plot + geom_sf(data  = borders,
                              fill  = "transparent",
                              color = borders_color,
                              size  = borders_size)

    if (!is.null(borders_txt_field)) {
      if (borders_txt_field %in% names(borders)) {

        centroids <- st_centroid(borders)
        centroids <- do.call(rbind, st_geometry(centroids)) %>%
          as_tibble() %>% setNames(c("lon","lat"))
        borders <- borders %>%
          mutate(lon = centroids$lon, lat = centroids$lat)
        plot <- plot + geom_text(data = borders,
                                 aes_string(label = borders_txt_field,
                                            x = "lon", y = "lat"),
                                 size = borders_txt_size,
                                 color = borders_txt_color)
      }
    }
  }


  # _________________________________________________________________________
  # Final adjustements on margins, etc.                                  ####
  plot <- plot + theme(plot.margin = margin(0.1,0.1,0.1,0.1, unit = "cm"),
                       panel.ontop = TRUE,
                       panel.background = element_rect(fill = "transparent"))

  #   _________________________________________________________________________
  #   Add scalebar                                                          ####

  if (scalebar) {

    plot <- plot +
      sprawl_scalebar(in_vect,
                      scalebar_dist  = scalebar_dist,
                      location = scalebar_pos,
                      x.min = xlims[1], x.max = xlims[2],
                      y.min = ylims[1], y.max = ylims[2],
                      st.size = 3.5,
                      st.bottom = FALSE, model = NULL,
                      st.dist = scalebar_txt_dist)

    #   _________________________________________________________________________
    # Center the title - can be overriden in case after plot completion      ####
    plot <- plot +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "transparent"))

    if (!show_grid) {
      plot <- plot + theme(
        panel.grid.major = element_line(size = 0, color = "transparent"))
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
  }

  plot + coord_sf(xlim = xlims, ylim = ylims)

}
