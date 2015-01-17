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
    return (paste(paste(names(x), paste0("'", x, "'"), sep = " = "), collapse = ", "))
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
#' Returns a named vector of the default category colours to use when
#' plotting water quality index values.
#' @return named character vector of colours for water quality index categories
#' @seealso \code{\link{calc_wqis}}
#' @examples
#' get_category_colours()
#' @export
get_category_colours <- function () {
  c(Excellent = "#081d58", Good = "#225ea8", Fair = "#41b6c4",
    Marginal = "#c7e9b4", Poor = "#edf8b1")
}

#' Plot Water Quality Indices
#'
#' Creates ggplot2 object with
#' y-limits expanded to include 0 and 100 and defined fill scale
#' for the WQI values corresponding by default to get_category_colours()
#'
#' @param data data.frame to plot
#' @param x string of column in data to plot on x axis
#' @param size number of point size or string of column in data
#' to plot size of points
#' @param shape integer of point shape (permitted values are 21 to 25)
#' or string of column in data to plot shape of points
#' @param theme ggplot theme
#' @param palette named character vector of palette for fill of points
#' @return ggplot2 object
#' @seealso \code{\link{get_category_colours}}
#' @examples
#' library(ggplot2)
#'
#' data(ccme)
#' plot_wqis(calc_wqis(ccme))
#'
#' \dontrun{
#'  demo(ccme)
#' }
#' @export
plot_wqis <- function (
  data, x = "Tests", size = 3, shape = 21, theme = theme_wqis(),
  palette = getOption("wqbc.category_colours", get_category_colours())) {

  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.number(size) || is.string(size))
  assert_that(is.count(shape) || is.string(shape))
  assert_that(ggplot2::is.theme(theme))
  assert_that(is.character(palette))

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
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::ylab("Water Quality Index") + theme

  gp <- gp + aes_string_point(
    head = "ggplot2::geom_point(ggplot2::aes_string(fill = 'Category'",
    size = size, shape = shape)

  if(is.string(shape))
    gp <- gp + ggplot2::scale_shape_manual(values = shape_values())
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
#' @param size number of point size or string of column in data to plot size of points
#' @param shape integer of point shape (permitted values are 21 to 25) or string of column in data to plot shape of points
#' @param fill integer of point fill or string of column in data to plot fill of points
#' @param theme ggplot theme
#' @param drop flag indicating whether to drop duplicated rows to
#' avoid overplotting
#' @param input_proj a valid proj4string. Defaults to longlat/NAD83
#' (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"})
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
#' @export
plot_map <- function (data,  x = "Long", y = "Lat", size = 3, shape = 21, fill = 10,
                      theme = theme_map(), drop = TRUE, input_proj = NULL) {

  assert_that(is.data.frame(data))
  assert_that(is.string(x))
  assert_that(is.string(y))
  assert_that(is.number(size) || is.string(size))
  assert_that(is.count(shape) || is.string(shape))
  assert_that(is.count(fill) || is.string(fill))
  assert_that(ggplot2::is.theme(theme))
  assert_that(is.flag(drop) && noNA(drop))
  assert_that(is.null(input_proj) || is.string(input_proj))

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  columns <- unique(c(x, y, ifelse(is.string(size), size, x),
                      ifelse(is.string(shape), shape, x),
                      ifelse(is.string(fill), fill, x)))

  check_columns(data, columns)

  if(is.count(shape) && !shape %in% shape_values()) {
    stop("Shape must be a character vector or ",
         punctuate_strings(shape_values()), ".")
  }
  if(drop)
    data <- unique(data[columns])

  data <- proj_bc(data, x = x, y = y, input_proj = input_proj)

  gp <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_polygon(
      data = wqbc::map,
      ggplot2::aes_string(x = "Long", y = "Lat", group = "Group"),
      fill = "grey80", size = 0.5, colour = "grey50"
    ) + ggplot2::coord_fixed() + theme

  gp <- gp + aes_string_point(size = size, shape = shape, fill = fill)

  if(is.string(shape))
    gp <- gp + ggplot2::scale_shape_manual(values = shape_values())
  gp
}

#' Plot Map of Water Quality Indices
#'
#' Creates ggplot2 object with map polygon,
#' coord_fixed and theme_minimal and fill based
#' on WQI categories.
#'
#' @inheritParams plot_wqis
#' @param y string of column in data to plot on y axis
#' @param drop flag indicating whether to drop duplicated rows to
#' avoid overplotting
#' @param input_proj a valid proj4string. Defaults to longlat/NAD83
#' (\code{"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"})
#' @examples
#' \dontrun{
#'  demo(fraser)
#' }
#' @export
plot_map_wqis <- function (
  data,  x = "Long", y = "Lat", size = 3, shape = 21,
  theme = theme_map(), drop = TRUE, input_proj = NULL,
  palette = getOption("wqbc.category_colours", get_category_colours())) {

  assert_that(is.character(palette))

  gp <- plot_map( data = data, x = x, y = y, size = size, shape = shape,
                  fill = "Category", theme = theme, drop = drop,
                  input_proj = input_proj)

  gp + ggplot2::scale_fill_manual(values = palette)
}
