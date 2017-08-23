#' @title plot_rast_gg
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param band_names PARAM_DESCRIPTION
#' @param bands_to_plot PARAM_DESCRIPTION
#' @param xlims PARAM_DESCRIPTION, Default: NULL
#' @param ylims PARAM_DESCRIPTION, Default: NULL
#' @param basemap PARAM_DESCRIPTION, Default: NULL
#' @param zoomin PARAM_DESCRIPTION, Default: -1
#' @param scalebar PARAM_DESCRIPTION, Default: TRUE
#' @param scalebar_dist PARAM_DESCRIPTION, Default: NULL
#' @param na.color PARAM_DESCRIPTION, Default: 'transparent'
#' @param na.value PARAM_DESCRIPTION, Default: NA
#' @param palette_type PARAM_DESCRIPTION, Default: 'gradient'
#' @param palette PARAM_DESCRIPTION, Default: NULL
#' @param labels PARAM_DESCRIPTION, Default: NULL
#' @param no_labels PARAM_DESCRIPTION, Default: TRUE
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  in_rast <- raster::stack(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "osm",
#'                   palette_type = "diverging",
#'                   no_labels = T,
#'                   na.value = 0,
#'                   zoomin = 0, title = "OLI", subtitle = "Band 2")
#'
#'  plot_rast_gg(in_rast, basemap = "stamenbw",
#'                   palette_type = "diverging",
#'                   no_labels = F,
#'                   na.value = 0, transparency = 0.2,
#'                   zoomin = 0, title = "OLI - 15/06/2017",
#'                    subtitle = "Band 2 - Green")
#'
#'  in_rast <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'                           package = "sprawl.data")
#'  plot_rast_gg(in_rast, basemap = "cartolight",
#'                   palette_type = "diverging",
#'                   no_labels = F, zoomin = -1,
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
#' @importFrom ggsn scalebar
#' @importFrom ggspatial geom_osm
#' @importFrom raster stack

plot_rast_gg <- function(
  in_rast,
  band_names     = NULL, bands_to_plot = NULL, nrows = NULL,
  xlims          = NULL, ylims = NULL,
  basemap        = NULL, zoomin = -1,
  scalebar       = TRUE, scalebar_dist = NULL,
  transparency   = 0,
  na.color       = "transparent", na.value = NA,
  palette_type   = "gradient", palette = NULL, labels = NULL,
  no_labels      = TRUE,
  title          = NULL, subtitle = NULL
) {

  #TODO find a way to avoid "require"
  #TODO Implement checks on input arguments (e.g., bands_to_plot, band_names)
  #TODO Verify possibility to have a "satellite" basemap
  #TODO Add MaxPixels
  require(ggspatial)
  x <- y <- value <- NULL

  assertthat::assert_that(palette_type %in% c("categorical", "gradient",
                                              "diverging"),
                          msg = "Invalid palette_type. Aborting!")

  #   __________________________________________________________________________
  #   Set default palettes for different categories                         ####
  def_palettes <- list(categorical = "Set1",
                       gradient    = "Greens",
                       diverging   = "RdYlGn")

  if (is.null(palette)) {
    palette <- as.character(def_palettes[palette_type])
  }

  if (!is.null(bands_to_plot)) {

    in_rast <- in_rast[[bands_to_plot]]

  }

  #   __________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                ####

  rastinfo <- get_rastinfo(in_rast)

  if (is.null(nrows)){
   nrows <- rastinfo$nbands
  }

  if (any(rastinfo$fnames == "")) {
    rastfile <- cast_rast(in_rast, "rastfile")
    in_rast <- raster::stack(rastfile)
  }
  rastinfo$fnames <- get_rastinfo(in_rast)$fnames

  if (!is.null(basemap)) {

    in_rast <- gdalUtils::gdalwarp(rastinfo$fnames,
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

  plot_gg <- ggplot2::ggplot(in_rast_fort) +
    ggplot2::scale_x_continuous(xlab,
                                expand = expand_scale(mult = c(0.02,0.02)),
                                limits = c(xlims[1], xlims[2])) +
    ggplot2::scale_y_continuous(ylab,
                                expand = expand_scale(mult = c(0.02,0.02)),
                                limits = c(ylims[1], ylims[2])) +
    ggplot2::ggtitle(title, subtitle = subtitle)

  #   ____________________________________________________________________________
  #   add background map - need to do this here to prevent shadowing          ####

  if (!is.null(basemap)) {
    plot_gg <- plot_gg + ggspatial::geom_osm(zoomin = zoomin,
                                             type   = basemap,
                                             progress = "none")
  }

  #   ____________________________________________________________________________
  #   add the raster layer          ####

  plot_gg <- plot_gg +
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = value),
                         alpha = 1 - transparency)

  #   ____________________________________________________________________________
  #   Modify the palette according to varaible type and palette               ####

  if (palette_type == "categorical") {
    plot_gg <- plot_gg +
      ggplot2::scale_fill_brewer(type = "qual", palette = palette)
  } else {
    if (palette_type == "sequential") {
      plot_gg <- plot_gg +
        ggplot2::scale_fill_distiller(type = "seq", palette = palette)
    } else {
      plot_gg <- plot_gg +
        ggplot2::scale_fill_distiller(type = "div", palette = palette,
                                      direction = 1)
    }
  }

  #   ____________________________________________________________________________
  #   Add scalebar               ####

  if (scalebar) {
    # ggplot2::coord_cartesian(xlim = xlims, ylim = ylims) +
    plot_gg <- plot_gg +
      ggsn::scalebar(dd2km = FALSE, dist = scalebar_dist,
                     x.min = xlims[1], x.max = xlims[2],
                     y.min = ylims[1], y.max = ylims[2],
                     location = "bottomright", st.size = 3.5,
                     st.bottom = FALSE, model = NULL,
                     st.dist = 0.025)
  }



  #   ____________________________________________________________________________
  #   Finalize and return               ####

  plot_gg <- plot_gg +
    ggplot2::coord_fixed()

  if (no_labels) {

    plot_gg <- plot_gg + ggplot2::theme_light() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

    plot_gg <- plot_gg + facet_wrap(~band, nrow = nrows)

  return(plot_gg)
}

