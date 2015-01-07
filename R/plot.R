#' Plot Water Quality Indices
#'
#' Creates ggplot2 object with map polygon,
#' limits expanded to include 0 and 100 and colour scale
#' with values get_category_colours()
#'
#' @param data data.frame to plot
#' @param x string of column in data to plot on x axis
#' @param size string of column in data to plot size of points
#' @param shape string of column in data to plot shape of points
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
#' wqis$Year <- year(wqis$Date)
#' wqis$Dayte <- wqis$Date
#' year(wqis$Dayte) <- 2000
#' plot_wqis(wqis, x = "Dayte", size = "Tests") +
#' facet_wrap(~Year) + xlab("Day of the Year") + theme_bw()
#'
#' @export
plot_wqis <- function (data, x = "Tests", size = NULL, shape = NULL) {
  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.null(size) || is.string(size))
  assert_that(is.null(shape) || is.string(shape))

  if(!"WQI" %in% colnames(data)) stop("data must contain WQI column")
  if(!"Category" %in% colnames(data)) stop("data must contain Category column")
  if(!x %in% colnames(data)) stop("data must contain ", x ," column")
  if(is.string(size) && !size %in% colnames(data)) stop("data must contain ", size ," column")
  if(is.string(shape) && !shape %in% colnames(data)) stop("data must contain ", shape ," column")

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ggplot2:: ggplot(data = data, ggplot2::aes_string(x = x, y = "WQI")) +
    ggplot2::geom_point(ggplot2::aes_string(colour = "Category", size = size, shape = shape)) +
    ggplot2::expand_limits(y = c(0, 100)) +
    ggplot2::scale_colour_manual(values = get_category_colours()) +
    ggplot2::ylab("Water Quality Index")
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
#' @param input_proj a valid proj4string. Defaults to longlat/NAD83 (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"})
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(sp)
#' library(rgdal)
#'
#' data(fraser)
#' plot_map(fraser)
#' plot_map(fraser, colour = "SiteID")
#'
#' library(lubridate)
#'
#' fraser$Year <- year(fraser$Date)
#' plot_map(fraser) + facet_wrap(~Year)
#'
#' @export
plot_map <- function (data,  x = "Longitude", y = "Latitude", colour = NULL,
                      shape = NULL, input_proj = NULL) {
  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.string(y))
  assert_that(is.null(colour) || is.string(colour))
  assert_that(is.null(shape) || is.string(shape))
  assert_that(is.null(input_proj) || is.string(input_proj))

  if(!x %in% colnames(data)) stop("data must contain ", x ," column")
  if(!y %in% colnames(data)) stop("data must contain ", y ," column")
  if(is.string(colour) && !colour %in% colnames(data)) stop("data must contain ", colour ," column")
  if(is.string(shape) && !shape %in% colnames(data)) stop("data must contain ", shape ," column")

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ## Get unique site locations so we don't do lots of overplotting
  unique_rows <- rownames(unique(data[c(x,y,colour,shape)]))
  data <- data[unique_rows,]

  data <- proj_bc(data, x = x, y = y, input_proj = input_proj)

  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_polygon(
      data = wqbc::map,
      ggplot2::aes_string(x = "Longitude", y = "Latitude", group = "Group"),
      fill = "grey80", size = 0.5, colour = "grey50"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::geom_point(ggplot2::aes_string(colour = colour, shape = shape)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()
    )
}
