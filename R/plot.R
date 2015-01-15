#' Plot Water Quality Indices
#'
#' Creates ggplot2 object with map polygon,
#' limits expanded to include 0 and 100 and colour scale
#' with values get_category_colours()
#'
#' @param data data.frame to plot
#' @param x string of column in data to plot on x axis
#' @param size number of size or string of column in data to plot size of points
#' @param shape integer of shape (permitted values are 21 to 25) or string of column in data to plot shape of points
#' @param theme ggplot theme
#' @inheritParams get_category_colours
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#'
#' data(ccme)
#' plot_wqis(calc_wqis(ccme))
#'
#' wqis <- calc_wqis(ccme, by = "Date")
#' plot_wqis(wqis)
#' plot_wqis(wqis, x = "Date")
#'
#' library(lubridate)
#'
#' wqis <- calc_wqis(ccme, by = "Date")
#' wqis$Year <- year(wqis$Date)
#' wqis$Dayte <- wqis$Date
#' year(wqis$Dayte) <- 2000
#' plot_wqis(wqis, x = "Dayte", size = "Tests") +
#' facet_wrap(~Year) + xlab("Day of the Year") + theme_bw()
#'
#' test <- data.frame(WQI = seq(0, 100, by = 5))
#' test$Category = categorize_wqi(test$WQI)
#' for (palette in c("default", "blue")) {
#'  print(plot_wqis(test, x = "WQI", palette = palette) + xlab(palette))
#' }
#' @export
plot_wqis <- function (data, x = "Tests", size = 3, shape = 21, palette = "default", theme = theme_wqis()) {
  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.number(size) || is.string(size))
  assert_that(is.count(shape) || is.string(shape))

  check_columns(data, unique(c("WQI", "Category", x,
                               ifelse(is.string(size), size, "WQI"),
                               ifelse(is.string(shape), shape, "WQI"))))

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  shape_values <- 21:25

  if(is.count(shape) && !shape %in% shape_values)
    stop("shape must be a character vector or ", punctuate_strings(shape_values))

  fill_values <- get_category_colours(palette = palette)

  gp <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = x, y = "WQI")) +
    ggplot2::expand_limits(y = c(0, 100)) +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::ylab("Water Quality Index") +
    theme

  point_colour <- "#081d58"

  if(is.string(size) && is.string(shape)) {
    gp <- gp + ggplot2::geom_point(
      ggplot2::aes_string(fill = "Category", size = size, shape = shape),
      colour = point_colour)
  } else if (!is.string(size) && !is.string(shape)) {
    gp <- gp + ggplot2::geom_point(
      ggplot2::aes_string(fill = "Category"), size = size, shape = shape,
      colour = point_colour)
  } else if (is.string(size)) {
    gp <- gp + ggplot2::geom_point(
      ggplot2::aes_string(fill = "Category", size = size), shape = shape,
      colour = point_colour)
  } else {
    gp <- gp + ggplot2::geom_point(
      ggplot2::aes_string(fill = "Category", shape = shape), size = size,
      colour = point_colour)
  }
  if(is.string(shape))
    gp <- gp + ggplot2::scale_shape_manual(values = shape_values)
  gp
}

#' Plot Map
#'
#' Creates ggplot2 object with map polygon,
#' coord_fixed and theme_minimal.
#'
#' @param data data.frame to plot
#' @param x string of column in data to plot on x axis
#' @param y string of column in data to plot on y axis
#' @param colour string of column in data to plot colour of points
#' @param shape string of column in data to plot shape of points
#' @param size number of size of points
#' @param theme ggplot theme
#' @param drop flag indicating whether to drop duplicated rows to
#' avoid overplotting
#' @param input_proj a valid proj4string. Defaults to longlat/NAD83 (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"})
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(sp)
#' library(rgdal)
#'
#' data(fraser)
#' plot_map(fraser)
#'
#' fraser <- fraser[fraser$SiteID %in% levels(fraser$SiteID)[1:5],]
#' plot_map(fraser, colour = "SiteID")
#'
#' library(lubridate)
#'
#' data(fraser)
#' fraser$Year <- year(fraser$Date)
#' fraser <- unique(fraser[c("Long","Lat","Year")])
#' fraser <- fraser[fraser$Year <= 1990,]
#' plot_map(fraser, drop = FALSE) + facet_wrap(~Year)
#'
#' @export
plot_map <- function (data,  x = "Long", y = "Lat", colour = NULL,
                      shape = NULL, size = 2, theme = theme_map(),
                      drop = TRUE, input_proj = NULL) {
  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.string(y))
  assert_that(is.null(colour) || is.string(colour))
  assert_that(is.null(shape) || is.string(shape))
  assert_that(is.number(size))
  assert_that(is.flag(drop) || noNA(drop))
  assert_that(is.null(input_proj) || is.string(input_proj))

  check_columns(data, unique(c(x, y,
                               ifelse(is.string(colour), colour, x),
                               ifelse(is.string(shape), shape, x))))

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  if(drop)
    data <- unique(data[c(x, y, colour, shape)])

  data <- proj_bc(data, x = x, y = y, input_proj = input_proj)

  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_polygon(
      data = wqbc::map,
      ggplot2::aes_string(x = "Long", y = "Lat", group = "Group"),
      fill = "grey80", size = 0.5, colour = "grey50"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::geom_point(ggplot2::aes_string(colour = colour, shape = shape),
                        size = size) +
    theme
}
