#' @title sprawl_scalebar
#' @description Adds a scale bar to maps created with `plot_rast_gg`.
#' @param in_obj `Raster` object passed to `plot_rast_gg` or `plot_vect` to which
#'  the scalebar should be added;
#' @param location `character ["bottomright" | "bottomleft" | "topleft" | "topright"]`
#'  indicating where the scalebar should be placed, Default: "bottomright"
#' @param scalebar_dist `numeric` distance in to represent with each segment of the scale bar.
#' @param height `numeric` value between 0 and 1 to indicate the height of the
#'   scale bar, as a proportion of the y axis, Default: 0.02
#' @param st.dist `numeric` value between 0 and 1 to indicate the distance
#'   between the scale bar and the scale bar text, as a proportion of the y axis,
#'   Default: 0.02
#' @param st.bottom `logical` If TRUE (default) the scale bar text is displayed
#'   at the bottom of the scale bar, if FALSE, it is displayed at the top.
#' @param st.size `numeric` value to indicate the scale bar text size. It is
#'   passed to the size argument of `ggplot::annotate` function.
#' @param st.color Text color for the scalebar, Default: 'black'
#' @param box.fill fill colors for the scalebar polygons,
#'   Default: c("black", "white")
#' @param box.color line color for the scalebar polygons, Default: 'black'
#' @param dd2km `logical` If TRUE it is assumed that map coordinates are in
#'   decimal degrees, if FALSE, it assumed they are in meters, Default: FALSE
#' @param model `character` choice of ellipsoid model ("WGS84", "GRS80", "Airy",
#'  "International", "Clarke", "GRS67"). Ignored if dd2km is FALSE.
#' @param anchor `named numeric(2)` with coordinates to control the
#'   symbol position. For \code{location = "topright"}, \code{anchor}
#'   defines the coordinates of the symbol's topright corner and so forth.
#'   The x coordinate must be named as x and the y coordinate as y.
#' @param x.min if `data` is not defined, number with the minimum x coordinate.
#' @param x.max if `data` is not defined, number with the maximum x coordinate.
#' @param y.min if `data` is not defined, number with the minimum y coordinate.
#' @param y.max if `data` is not defined, number with the maximum y coordinate.
#' @param facet.var if faceting, character vector of variable names used for
#'   faceting. This is useful for placing the scalebar only in one facet and must
#'   be used together with `facet.lev`.
#' @param facet.lev character vector with the name of one level for each
#'   variable in `facet.var`. The scale bar will be drawn only in the `facet.lev`
#'   facet.
#' @param units DESCRIPTION
#' @details Simple modifications on the `scalebar` function in package `ggsn`
#' @rdname sprawl_scalebar
#' @author - Original code by Oswaldo Santos Baquero - ggsn: North Symbols and
#'  Scale Bars for Maps Created with 'ggplot2' or 'ggmap'. R package version
#'  0.4.3. https://github.com/oswaldosantos/ggsn
#'  - Slight modifications to adapt it to `sprawl` needs by Lorenzo Busetto:
#'    - Removed measure units text from all but the last numeric label;
#'    - Added support for non-metric (e.g., feet) projections by adding the `units`
#'      argument and changing the text accordingly;
#'    - fontface to bold in the scalebar to improve visibility;
#'    - automatic computation of distance if `dist` is not passed
#' @export
#' @importFrom ggplot2 geom_polygon geom_text
#' @importFrom maptools gcDestination
#' @importFrom sf st_bbox
#' @importFrom utils tail

sprawl_scalebar <- function(
  in_obj, location = "bottomright", scalebar_dist = NULL, height = 0.02,
  st.dist = 0.010, st.bottom = TRUE, st.size = 3.5, st.color = "black",
  box.fill = c("black", "white"), box.color = "black", dd2km = FALSE,
  model, x.min, x.max, y.min, y.max, anchor = NULL, facet.var = NULL,
  facet.lev = NULL, units = "m")
{

  label <- NULL

  # Get useful coordinates to place the scalebar
  coords_bar <- data.frame(long = c(x.min, x.max),
                           lat  = c(y.min, y.max))
  #   __________________________________________________________________________
  #   If no scalebar dist passed, compute automatically from longitude     ####
  #   range
  units <- get_projunits(get_proj4string(in_obj))

  dd2km <- ifelse(units == "dec.degrees", TRUE, FALSE)

  if (is.null(scalebar_dist)) {
    if (units != "dec.degrees") {
      km_extent  <- round(x.max - x.min) / 1000
      dist <- round(round(km_extent / 10) / 5) * 5
      if (dist == 0) {
        dist <- ((km_extent/10) / 5) * 5
      }
    } else {
      deg2rad <- function(deg) {(deg * pi) / (180)}
      a <- sin(0.5 * (deg2rad(y.max) - deg2rad(y.min)))
      b <- sin(0.5 * (deg2rad(x.max) - deg2rad(x.min)))
      km_extent     <- 12742 * asin(sqrt(a * a +
                                           cos(deg2rad(y.min)) *
                                           cos(deg2rad(y.max)) * b * b))
      dist <- round(round(km_extent / 10) / 5) * 5
      }
  } else {
    dist <- scalebar_dist
  }

  if (is.null(dd2km)) {
    stop("dd2km should be logical.")
  }

  # if (any(class(in_obj) %in% "sf")) {
  #   x.min <- sf::st_bbox(in_obj)["x.min"]
  #   x.max <- sf::st_bbox(in_obj)["x.max"]
  #   y.min <- sf::st_bbox(in_obj)["y.min"]
  #   y.max <- sf::st_bbox(in_obj)["y.max"]
  # }
  # else {
  #   x.min <- min(in_obj$long)
  #   x.max <- max(in_obj$long)
  #   y.min <- min(in_obj$lat)
  #   y.max <- max(in_obj$lat)
  # }
  if (location == "bottomleft") {
    if (is.null(anchor)) {
      x <- x.min + (x.max - x.min) / 20
      y <- y.min
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "bottomright") {
    if (is.null(anchor)) {
      # x <- x.max - (x.max - x.min) / 15
      # y <- y.min + (y.max - y.min) / 40
      x <- x.max - (x.max - x.min) / 15
      y <- y.min + (y.max - y.min) / 40
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (location == "topleft") {
    if (is.null(anchor)) {
      x <- x.min
      y <- y.max
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "topright") {
    if (is.null(anchor)) {
      x <- x.max
      y <- y.max
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }

  if (!st.bottom) {
    st.dist <- y + (y.max - y.min) * (height + st.dist)
  }
  else {
    st.dist <- y - (y.max - y.min) * st.dist
  }
  height <- y + (y.max - y.min) * height


  if (dd2km) {
    break1 <- maptools::gcDestination(lon = x, lat = y, bearing = 90 *
                                      direction,
                                      dist = dist,
                                      dist.units = "km",
                                      model = model)[1,1]
    break2 <- maptools::gcDestination(lon = x, lat = y,
                                      bearing = 90 * direction,
                                      dist = dist * 2,
                                      dist.units = "km",
                                      model = model)[1,1]
  }
  else {
    if (location == "bottomleft" | location == "topleft") {
      if (units == "m") {
        break1 <- x + dist * 1000
        break2 <- x + dist * 2000
      }

      if (units == "ft") {
        break1 <- x + dist * 3280.9
        break2 <- x + dist * 3280.9 * 2
      }
    }
    else {
      if (units == "m") {
        break1 <- x - dist * 1000
        break2 <- x - dist * 2000
      }

      if (units == "ft") {
        break1 <- x - dist * 3280.9
        break2 <- x - dist * 3280.9 * 2
      }
    }
  }

  box1 <- data.frame(x = c(x, x, rep(break1, 2), x),
                     y = c(y, height, height, y, y), group = 1)
  box2 <- data.frame(x = c(rep(break1, 2), rep(break2, 2), break1),
                     y = c(y, rep(height, 2), y, y), group = 1)
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      box1[, facet.var[i]] <- facet.lev[i]
      box2[, facet.var[i]] <- facet.lev[i]
    }
  }
  legend <- cbind(text = c(0, dist, dist * 2), row.names = NULL)
  gg.box1 <- ggplot2::geom_polygon(data = box1, aes(x, y),
                          fill = utils::tail(box.fill, 1),
                          color = utils::tail(box.color, 1))
  gg.box2 <- ggplot2::geom_polygon(data = box2, aes(x, y),
                          fill = box.fill[1],
                          color = box.color[1])
  x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
  if (location == "bottomright" | location == "topright") {
    x.st.pos <- rev(x.st.pos)
  }

  legend2 <- cbind(coords_bar[1:3, ], x = x.st.pos, y = st.dist,
                   label = format(legend[, "text"], digits = 3),
                   stringsAsFactors = FALSE)
  legend2$label[3] <- paste(legend2$label[3], "km")

  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      legend2[, facet.var[i]] <- facet.lev[i]
    }
  }
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    gg.legend <- ggplot2::geom_text(data = legend2, aes(x, y, label = label),
                           size = st.size, color = st.color,
                           fontface = "bold")
  }
  else {
    gg.legend <- ggplot2::geom_text(data = legend2, aes(x, y, label = label),
                           size = st.size, color = st.color,
                           fontface = "bold")
  }
  return(list(gg.box1, gg.box2, gg.legend))
}
