#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_rast PARAM_DESCRIPTION
#' @param xlims PARAM_DESCRIPTION, Default: NULL
#' @param ylims PARAM_DESCRIPTION, Default: NULL
#' @param basemap PARAM_DESCRIPTION, Default: NULL
#' @param zoomin PARAM_DESCRIPTION, Default: -1
#' @param scalebar PARAM_DESCRIPTION, Default: TRUE
#' @param scalebar_dist PARAM_DESCRIPTION, Default: NULL
#' @param na.color PARAM_DESCRIPTION, Default: 'transparent'
#' @param palette_type PARAM_DESCRIPTION, Default: 'gradient'
#' @param palette PARAM_DESCRIPTION, Default: NULL
#' @param no_labels PARAM_DESCRIPTION, Default: TRUE
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  in_rast <- raster::stack(system.file("extdata/OLI_test", "oli_multi_1000.tif",
#'                                   package = "sprawl.data"))
#'  plot_rast_gg(in_rast, basemap = "cartolight",
#'                   palette_type = "diverging",
#'                   no_labels = T,
#'                   na.value = 0,
#'                   zoomin = 0)
#'
#' in_rast <- system.file("extdata/REYE_test", "REYE_2016_185_gNDVI.tif",
#'  package = "sprawl.data")
#'  plot_rast_gg(in_rast, basemap = "cartolight",
#'                   palette_type = "diverging",
#'                   no_labels = T,
#'                   na.value = 0,
#'                   zoomin = 0)
#' }
#' @rdname plot_rast_gg
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 fortify ggplot theme_bw xlim ylim ggtitle geom_raster aes
#'  scale_fill_brewer coord_fixed
#' @importFrom ggsn scalebar
#' @importFrom sf st_as_sf st_set_crs st_transform st_coordinates
#' @importFrom magrittr "%>%"
#' @import ggspatial

plot_rast_gg <- function(in_rast,
                         xlims          = NULL, ylims = NULL,
                         basemap        = NULL, zoomin = -1,
                         scalebar       = TRUE, scalebar_dist = NULL,
                         na.color       = "transparent", na.value = NA,
                         palette_type   = "gradient", palette = NULL,
                         no_labels      = TRUE,
                         title          = NULL, subtitle = NULL) {

  assertthat::assert_that(palette_type %in% c("categorical", "gradient", "diverging"),
                          msg = "Invalid palette_type. Aborting!")

  #   ____________________________________________________________________________
  #   Set default palettes for different categories                           ####
  def_palettes <- list(categorical = "Set1",
                       gradient    = "Greens",
                       diverging   = "RdYlGn")

  if (is.null(palette)) {
    palette <- as.character(def_palettes[palette_type])
  }


  #   ____________________________________________________________________________
  #   Reproject to 3857 to allow overlap with background map                  ####

  if (!is.null(basemap)) {

    rastinfo <- get_rastinfo(in_rast)

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

  ext <- get_extent(in_rast)
  in_rast_fort <- ggplot2::fortify(in_rast) %>%
    data.table::as.data.table()

  if (!is.na(na.value)) {
    in_rast_fort[band1 == na.value] <- NA
  }

  #   ____________________________________________________________________________
  #   if transparent NA, remove the NAs from the data to speed-up rendering   ####

  if (na.color == "transparent")  {
    in_rast_fort <- na.omit(in_rast_fort, 3)
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
    km_extent  <- round(diff(xlims)/1000)
    scalebar_dist <- round(km_extent/100*10)
  }

  if (palette_type == "categorical") {
    in_rast_fort[[3]] <- factor(in_rast_fort[[3]])
  }

  #   ____________________________________________________________________________
  #   Initialize the plot                                                     ####

  plot_gg <- ggplot2::ggplot(in_rast_fort) +
    ggplot2::xlim(xlims[1], xlims[2]) +
    ggplot2::ylim(ylims[1], ylims[2]) +
    ggplot2::ggtitle(title, subtitle = subtitle)

  #   ____________________________________________________________________________
  #   add background map - need to do this here to prevent shadowing          ####

  if (!is.null(basemap)) {
    plot_gg <- plot_gg + ggspatial::geom_osm(zoomin = zoomin,
                                             type   = basemap)
  }

  #   ____________________________________________________________________________
  #   add the raster layer          ####

  plot_gg <- plot_gg +
    ggplot2::geom_raster(aes(x, y, fill = band1))

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
        ggplot2::scale_fill_distiller(type = "div", palette = palette, direction = -1)
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
                     location = "bottomright", st.size = 3,
                     st.bottom = FALSE, model = NULL,
                     st.dist = 0.03)
  }



  #   ____________________________________________________________________________
  #   Finalize and return               ####

  plot_gg <- plot_gg +
    ggplot2::coord_fixed()

  if (no_labels) {

    plot_gg <- plot_gg + ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  return(plot_gg)
}

