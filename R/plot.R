#' Plot Water Quality Indices
#'
#' Creates ggplot2 object with map polygon,
#' limits expanded to include 0 and 100 and color scale
#' with values get_category_colors()
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
  if(is.string(shape) && !size %in% colnames(data)) stop("data must contain ", shape ," column")

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ggplot2:: ggplot(data = data, ggplot2::aes_string(x = x, y = "WQI")) +
    ggplot2::geom_point(ggplot2::aes_string(color = "Category", size = size, shape = shape)) +
    ggplot2::expand_limits(y = c(0, 100)) +
    ggplot2::scale_color_manual(values = get_category_colors()) +
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
#' @param input_proj a valid proj4string. Defaults to longlat/NAD83 (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"})
#' @return ggplot2 object
#' @examples
#' library(sp)
#' library(rgdal)
#' library(ggplot2)
#'
#' data(fraser)
#' plot_map(fraser)
#'
#' @export
plot_map <- function (data,  x = "Longitude", y = "Latitude", input_proj = NULL) {
  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.string(y))

  if(!requireNamespace("sp", quietly = TRUE))
    stop("sp package not installed")

  if(!requireNamespace("rgdal", quietly = TRUE))
    stop("rgdal package not installed")

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ## Get unique site locations so we don't do lots of overplotting
  unique_rows <- rownames(unique(data[c(x,y)]))
  data <- data[unique_rows,]

  ## Assign and transform the coordinate system/projection to match the base BC map
  if (is.null(input_proj)) {
    input_proj <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  }
  output_proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  sp::coordinates(data) <- c(x,y)
  sp::proj4string(data) <- sp::CRS(input_proj)
  data <- sp::spTransform(data, sp::CRS(output_proj))
  data <- as.data.frame(data)

  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_polygon(data = wqbc::map,
                          ggplot2::aes_string(x = "Longitude", y = "Latitude", group = "Group"),
                 fill = "grey80", size = 0.5, colour = "grey50") +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
}
