# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

point_colour <- function () {
  "#081d58"
}

shape_values <- function () {
  21:25
}

paste_sep_collapse <- function (x, string = FALSE) {
  if(!length(x))
    return (NULL)

  if(string)
    return (paste(paste(names(x), paste0("'", x, "'"), sep = " = "),
                  collapse = ", "))
  paste(paste(names(x), x, sep = " = "), collapse = ", ")
}

is_end_open_bracket <- function (x) {
  substr(x,nchar(x), nchar(x)) == "("
}

aes_string_point_expr <- function (head, tail, ...) {
  args <- list(...)

  string <- sapply(args, is.string)

  strings <- paste_sep_collapse(args[string], string = TRUE)
  values <- paste_sep_collapse(args[!string])
  comma_strings <- ifelse(length(strings) && !is_end_open_bracket(head), ", ", "")
  comma_values <- ifelse(length(values), ", ", "")

  expr <- paste0(head, comma_strings , strings, ")", comma_values, values , tail)
  expr
}

aes_string_point <- function (head = "ggplot2::geom_point(ggplot2::aes_string(",
                              tail = ", colour = point_colour())", ...) {

  expr <- aes_string_point_expr(head = head, tail = tail, ...)
  expr <- paste("expr <- ", expr)
  eval(parse(text = expr))
  expr
}

#' Get Category Colours
#'
#' Returns a named vector of the WQI category colours used for plotting.
#' @export
get_category_colours <- function () {
  c(Excellent = "#081d58", Good = "#225ea8", Fair = "#41b6c4",
    Marginal = "#c7e9b4", Poor = "#edf8b1")
}

#' Plot Water Quality Indices
#'
#' Creates a ggplot2 scatterplot object with the
#' y-limits expanded to include 0 and 100. Water Quality Index categories are
#' indicated by the fill colour of points.
#'
#' @param data A data.frame of WQI values to plot.
#' @param x A string of the column in data to plot on the x axis.
#' @param size A number of the point size or string of the column in data
#' to represent by the size of points.
#' @param shape An integer of the point shape (permitted values are 21 to 25)
#' or string of the column in data to represent by the shape of points.
#' @examples
#' \dontrun{
#'  demo(fraser)
#' }
#' @seealso \code{\link{plot_map_wqis}}
#' @export
plot_wqis <- function (data, x = "Tests", size = 3, shape = 21) {

  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.number(size) || is.string(size))
  assert_that(is.count(shape) || is.string(shape))

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  columns <- unique(c(x, ifelse(is.string(size), size, x),
                      ifelse(is.string(shape), shape, x)))

  check_columns(data, columns)

  if(is.count(shape) && !shape %in% shape_values()) {
    stop("Shape must be a character vector or ",
         punctuate_strings(shape_values()), ".")
  }

  gp <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = x, y = "WQI")) +
    ggplot2::expand_limits(y = c(0, 100)) +
    ggplot2::scale_fill_manual(values = get_category_colours()) +
    ggplot2::ylab("Water Quality Index") + theme_wqis()

  gp <- gp + aes_string_point(
    head = "ggplot2::geom_point(ggplot2::aes_string(fill = 'Category'",
    size = size, shape = shape)

  if(is.string(shape))
    gp <- gp + ggplot2::scale_shape_manual(values = shape_values())
  gp
}

proj_bc <- function (data, x, y, input_proj = NULL) {

  if(!requireNamespace("sp", quietly = TRUE))
    stop("sp package not installed")

  if(!requireNamespace("rgdal", quietly = TRUE))
    stop("rgdal package not installed")

  if (is.null(input_proj)) {
    input_proj <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  }
  output_proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  sp::coordinates(data) <- c(x,y)
  sp::proj4string(data) <- sp::CRS(input_proj)
  data <- sp::spTransform(data, sp::CRS(output_proj))
  as.data.frame(data)
}

#' Plot Map
#'
#' Creates a ggplot2 object with a polygon of British Columbia.
#' If any columns are required
#' for additional layers in the plot or facetting then
#' they should be specified in the keep argument.
#'
#' @param data A data.frame with spatial information to map.
#' @param x A string of the column in data to plot on the x axis.
#' @param y A string of the column in data to plot on the y axis.
#' @param size A number of the point size or a string of the column in data
#' to represent by the size of points.
#' @param shape An integer of the point shape (permitted values are 21 to 25)
#' or a string of the column in data to represent by the shape of points.
#' @param fill An integer of the point fill colour or a string of the column in data to represent
#' by the fill colour of points.
#' @param keep An optional character vector indicating which columns
#' in addition to x and y to keep before dropping duplicated rows to
#' avoid overplotting.
#' @param input_proj An optional valid proj4string. Defaults to
#' (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"}).
#' @examples
#' library(ggplot2)
#' library(sp)
#' library(rgdal)
#'
#' data(fraser)
#' plot_map(fraser)
#'
#' \dontrun{
#'  demo(fraser)
#' }
#' @seealso \code{\link{plot_map_wqis}}
#' @export
plot_map <- function (data,  x = "Long", y = "Lat", size = 3, shape = 21, fill = 10,
                      keep = NULL, input_proj = NULL) {

  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.string(y))
  assert_that(is.number(size) || is.string(size))
  assert_that(is.count(shape) || is.string(shape))
  assert_that(is.count(fill) || is.string(fill))
  assert_that(is.null(keep) || is.character(keep))
  assert_that(is.null(input_proj) || is.string(input_proj))

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  columns <- unique(c(x, y, keep, ifelse(is.string(size), size, x),
                      ifelse(is.string(shape), shape, x),
                      ifelse(is.string(fill), fill, x)))

  check_columns(data, columns)

  if(is.count(shape) && !shape %in% shape_values()) {
    stop("Shape must be a character vector or ",
         punctuate_strings(shape_values()), ".")
  }
  data <- unique(data[columns])

  data <- proj_bc(data, x = x, y = y, input_proj = input_proj)

  gp <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_polygon(
      data = map,
      ggplot2::aes_string(x = "Long", y = "Lat", group = "Group"),
      fill = "grey80", size = 0.5, colour = "grey50"
    ) + ggplot2::coord_fixed() + theme_map()

  gp <- gp + aes_string_point(size = size, shape = shape, fill = fill)

  if(is.string(shape))
    gp <- gp + ggplot2::scale_shape_manual(values = shape_values())
  gp
}

#' Plot Map of Water Quality Index Categories.
#'
#' Creates a ggplot2 object with a polygon of British Columbia with
#' the Water Quality Index categories indicated by the fill colour of points.
#'
#' @inheritParams plot_wqis
#' @inheritParams plot_map
#' @examples
#' \dontrun{
#'  demo(fraser)
#' }
#' @seealso \code{\link{plot_wqis}} and \code{\link{plot_map}}
#' @export
plot_map_wqis <- function (
  data,  x = "Long", y = "Lat", size = 3, shape = 21, keep = NULL, input_proj = NULL) {

  gp <- plot_map( data = data, x = x, y = y, size = size, shape = shape,
                  fill = "Category", keep = keep,
                  input_proj = input_proj)

  gp + ggplot2::scale_fill_manual(values = get_category_colours())
}

plot_timeseries_by <- function(data, title = NULL, color, y0, messages) {
  if (!is.null(title)) check_string(title)

  if (!is_color(color)) check_cols(data, color)

  data %<>% dplyr::mutate_(Detected = ~Value >= DetectionLimit)
  data$Detected %<>% factor(levels = c(TRUE, FALSE))

  gp <- ggplot2::ggplot(data, ggplot2::aes_string(x = "Date", y = "Value"))

  if (!is.null(title)) gp <- gp + ggplot2::ggtitle(title)

  if (!is_color(color)) {
    if (all(is.na(data$Detected))) {
      gp <- gp + ggplot2::geom_point(ggplot2::aes_string(color = color))
    } else
      gp <- gp + ggplot2::geom_point(ggplot2::aes_string(color = color, alpha = "Detected"))
  } else {
    if (all(is.na(data$Detected))) {
      gp <- gp + ggplot2::geom_point(color = color)
    } else
      gp <- gp + ggplot2::geom_point(color = color, ggplot2::aes_string(alpha = "Detected"))
  }
  if (!all(is.na(data$Detected)))
    gp <- gp + ggplot2::scale_alpha_discrete(range = c(1, 0.1), drop = FALSE)
  if (y0) gp <- gp + ggplot2::expand_limits(y = 0)
  gp
}

plot_timeseries_fun <- function(data, by, color, y0, messages) {
  title <- paste(data[by][1,], collapse = " ")
  plot_timeseries_by(data, title = title, color = color, y0 = y0, messages = messages)
}

#' Plot Time Series Data
#'
#' If \code{by = NULL} plot_timeseries returns a ggplot object.
#' Otherwise it returns a list of ggplot objects.
#'
#' @param data A data frame of the data to plot.
#' @param by A character vector of the columns to plot the time series by.
#' @param color A string specifying the color for the points or if not a color the name of the column to color the points by.
#' @param y0 A flag indicating whether to expand the y-axis limits to include 0.
#' @param messages A flag indicating whether to print messages.
#' @export
#' @examples
#' plot_timeseries(ccme[ccme$Variable == "As",])
#' plot_timeseries(ccme, by = "Variable")
plot_timeseries <- function(data, by = NULL, color = "black", y0 = TRUE,
                            messages = getOption("wqbc.messages", default = TRUE)) {
  assert_that(is.null(by) || (is.character(by) && noNA(by)))

  check_string(color)
  check_flag(y0)
  check_flag(messages)

  check_by(by, colnames(data))
  if (is.null(data$DetectionLimit))
    data$DetectionLimit <- NA_real_
  check_class_columns(data, list("Date" = "Date", "Value" = "numeric", "DetectionLimit" = "numeric"))

  if (is.null(by)) {
    data %<>% plot_timeseries_by(color = color, y0 = y0, messages = messages)
  } else {
    data %<>% plyr::dlply(.variables = by, .fun = plot_timeseries_fun, by = by, color = color, y0 = y0, messages = messages)
  }
  data
}
