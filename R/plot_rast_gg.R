#' @title plot a raster object using ggplot
#' @description FUNCTION_DESCRIPTION
#' @param in_rast `Raster` object to be plotted. Both mono- and multi-band
#'   rasters are supported
#' @param band_names `character(nbands)`, array of band names. These will used
#'   to populate the "strips" above each plotted band. If NULL, bnames are
#'   retrieved from the input raster using `sprawl::get_rastinfo`, Default: NULL
#' @param bands_to_plot `numeric array`, array of band numbers to be plotted (in
#'   case `in_rast` is multi-band. If NULL, all bands are plotted separately
#'   using facet_wrap, Default: NULL
#' @param nrows `numeric`, number of rows used for plotting multiple bands,
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
#' @param basemap `character` If not NULL and valid, the selected basemap is
#'   used as background. For a list of valid values, see `rosm::osm.types()`,
#'   Default: NULL
#' @param zoomin `numeric`, Adjustement factor for basemap zoom. Negative values
#'   lead to less detailed basemap, but larger text. Default: 0
#' @param scalebar `logical` If TRUE, add a scalebar on the bottom right corner,
#'   Default: TRUE
#' @param scalebar_dist `numeric` Width of the scale bar (in km). If NULL, it
#'   is computed automatically on the basis of the range in x direction,
#'   Default: NULL
#' @param transparency `numeric [0,1]`, transparency of the raster layer. Higer
#'   values lead to higher transparency, Default: 0 (ignored if basemap == NULL)
#' @param na.color `character`, color to be used to plot NA values,
#'   Default: 'transparent'
#' @param na.value `numeric`, Additional values to be treatedas NA, Default: NA
#' @param palette_type `character` Type of brewer color ramp to be used. Possible
#'  values are `gradient`, `divergent` and `categorical`, Default: 'gradient'
#' @param palette name of the palette to be used. If NULL, the following
#'  defaults are used as a function of palette_type:
#'  - categorical --> "Set1"
#'  - gradient    --> "Greens"
#'  - diverging   --> "RdYlGn")
#'  Note that if a wrong palette name is specified, plot_rast_gg also reverts to
#'  the default values, Default: NULL
#' @param legend_type DESCREIPTION
#' @param labels `character` labels to be used in the legend if palette_type is
#'   "categorical". The number of labels must correspond to the number of
#'   unique values of the raster to be plotted. If NULL or not valid, the legend
#'   will use the raster values in the legend (see examples), Default: NULL
#' @param breaks `character` DESCRIPTIOS Default: NULL
#' @param no_axis `logical`, If TRUE, axis names and labels are suppressed,
#'   Default: FALSE
#' @param verbose `logical`, If FALSE, suppress processing message,
#'  Default: TRUE
#' @param title `character`, Title of the plot, Default: NULL
#' @param subtitle Subtitle of the plot, Default: NULL
#' @return a `ggplot`
#' @examples
#' \dontrun{
#'  in_rast <- raster::stack(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm",
#'                   palette_type = "diverging",
#'                   no_axis = T,
#'                   na.value = 0,
#'                   zoomin = 0, title = "OLI", subtitle = "Band 2")
#'
#'  plot_rast_gg(in_rast, basemap = "stamenbw",
#'                   palette_type = "diverging",
#'                   no_axis = F,
#'                   na.value = 0, transparency = 0.2,
#'                   zoomin = 0, title = "OLI - 15/06/2017",
#'                    subtitle = "Band 2 - Green")
#'
#'  in_rast <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data")
#'  plot_rast_gg(in_rast, basemap = "cartolight",
#'                   palette_type = "diverging",
#'                   no_axis = F, zoomin = -1,
#'                   na.value = 0, transparency = 0.5,
#'                   title = "gNDVI - 14/03/2016", subtitle = "From RapidEye")
#' }
#' @rdname plot_rast_gg
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom gdalUtils gdalwarp
#' @importFrom ggplot2 fortify ggplot scale_x_continuous
#' scale_y_continuous ggtitle geom_raster aes scale_fill_brewer
#' scale_fill_distiller coord_fixed theme_light theme element_blank
#' facet_wrap
#' @importFrom ggsn scalebar
#' @importFrom ggspatial geom_osm
#' @importFrom raster stack
#' @importFrom rosm osm.types
#' @importFrom stats na.omit
#' @importFrom magrittr %>%

plot_rast_gg <- function(in_rast,
                         band_names   = NULL, bands_to_plot = NULL, nrows = NULL, #nolint
                         xlims        = NULL, ylims = NULL,
                         zlims        = NULL, zlims_type = "vals",
                         basemap      = NULL, zoomin = 0,
                         scalebar     = TRUE, scalebar_dist = NULL,
                         transparency = 0,
                         na.color     = "transparent", na.value = NA,
                         palette_type = "gradient", palette = NULL,
                         legend_type  = NULL, labels = NULL, breaks = NULL, #nolint
                         no_axis      = TRUE, title = NULL, subtitle = NULL,
                         theme        = ggplot2::theme_bw(),
                         verbose      = TRUE) {

  #TODO find a way to avoid "require"
  #TODO Implement checks on input arguments (e.g., bands_to_plot, band_names)
  #TODO Verify possibility to have a "satellite" basemap
  #TODO Add MaxPixels
  requireNamespace("ggspatial")
  requireNamespace("ggplot2")
  x <- y <- value <- NULL

  assert_that(palette_type %in% c("categorical", "gradient", "diverging"),
              msg = "plot_rast_gg --> Invalid palette_type. Aborting!"
  )

  if (!is.null(basemap)) {
    assert_that(basemap %in% rosm::osm.types(),
                msg = "plot_rast_gg --> Invalid basemap name. Aborting!")
  }
  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####
  def_palettes  <- list(categorical = "Set1",
                       gradient    = "Greens",
                       diverging   = "RdYlGn")

  def_legtypes  <- list(categorical = "discrete",
                        gradient    = "continuous",
                        diverging   = "continuous")

  if (is.null(palette)) {
    palette <- as.character(def_palettes[palette_type])
  }

  if (is.null(legend_type)) {
    legend_type <- as.character(def_legtypes[palette_type])
  }

  if (!is.null(bands_to_plot)) {

    in_rast <- in_rast[[bands_to_plot]]

  }

  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  rastinfo <- get_rastinfo(in_rast, verbose = FALSE)

  if (is.null(nrows)) {
    nrows <- rastinfo$nbands
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
  #   If limits passed and zlims_type == percs, compute the quantiles

  if (is.null(band_names)) {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = rastinfo$bnames)
  } else {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]], labels = band_names)
  }

  if (!is.na(na.value)) {
    in_rast_fort[value == na.value] <- NA
  }

  #   ____________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering   ####

  if (na.color == "transparent")  {
    in_rast_fort <- na.omit(in_rast_fort, "value")
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

  #   ____________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from lonfgitude range ####
  if (is.null(scalebar_dist)) {
    km_extent     <- round(diff(xlims)/1000)
    scalebar_dist <- round(km_extent/100*10)
  }

  if (palette_type == "categorical") {

    if (is.null(labels)) {
      in_rast_fort[[4]] <- factor(in_rast_fort[[4]])
    } else {
      in_rast_fort[[4]] <- factor(in_rast_fort[[4]], labels = labels)
    }
  }

  #   ____________________________________________________________________________
  #   Initialize the plot                                                     ####

  if (rastinfo$units == "dec.degrees") {
    xlab = "Longitude"
    ylab = "Latitude"
  } else {
    xlab = paste0("Easting [",rastinfo$units,"]")
    ylab = paste0("Northing [",rastinfo$units,"]")
  }

  plot_gg <- ggplot2::ggplot(in_rast_fort) + theme +
    ggplot2::scale_x_continuous(xlab,
                                expand = expand_scale(mult = c(0.005,0.005)),
                                limits = c(xlims[1], xlims[2])) +
    ggplot2::scale_y_continuous(ylab,
                                expand = expand_scale(mult = c(0.005,0.005)),
                                limits = c(ylims[1], ylims[2])) +
    ggplot2::ggtitle(title, subtitle = subtitle)

  if (no_axis) {

    plot_gg <- plot_gg  +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  plot_gg <- plot_gg +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  #   ____________________________________________________________________________
  #   add background map - need to do this here to prevent shadowing          ####

  if (!is.null(basemap)) {

    osm_plot <- suppressMessages(
      ggspatial::geom_osm(zoomin = zoomin, type   = basemap, progress = "none")
    )
    plot_gg <- plot_gg + osm_plot
  }

  #   ____________________________________________________________________________
  #   add the raster layer          ####

  plot_gg <- plot_gg +
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = value),
                         alpha = 1 - transparency)

  #   __________________________________________________________________________
  #   Modify the palette according to varaible type and palette             ####

  if (palette_type == "categorical") {
    plot_gg <- plot_gg +
      ggplot2::scale_fill_brewer(type = "qual", palette = palette)
  } else {

    plot_gg <- plot_gg +
      ggplot2::scale_fill_distiller(
        "",
        limits = zlims, breaks = if(is.null(breaks)) {
          ggplot2::waiver()
        } else {
          breaks
        }, labels = if(is.null(labels)) {
          ggplot2::waiver()
        } else {
          labels
        }, type = ifelse(palette_type == "sequential", "seq", "div"),
        guide = ifelse(legend_type == "continuous", "colourbar", "legend"),
        palette = palette,
        direction = 1) +
      theme(legend.justification = "center",
            legend.box.spacing = grid::unit(0.5,"points"))
  }

  #   ____________________________________________________________________________
  #   Add scalebar               ####

  if (scalebar) {
    # ggplot2::coord_cartesian(xlim = xlims, ylim = ylims) +
    plot_gg <- plot_gg +
      sprawl::sprawl_scalebar(dd2km = FALSE, dist = scalebar_dist,
                              x.min = xlims[1], x.max = xlims[2],
                              y.min = ylims[1], y.max = ylims[2],
                              location = "bottomright", st.size = 3.5,
                              st.bottom = FALSE, model = NULL,
                              st.dist = 0.025, units = rastinfo$units)
  }



  #   ____________________________________________________________________________
  #   Finalize and return               ####

  plot_gg <- plot_gg +
    ggplot2::coord_fixed()


  plot_gg <- plot_gg + ggplot2::facet_wrap(~band, nrow = nrows)

  plot_gg
}

