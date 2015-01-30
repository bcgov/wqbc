standardise_values_units_variable <- function (x, messages) {
  codes <- wqbc_codes()
  codes <- dplyr::filter_(codes, ~Variable == x$Variable[1])
  stopifnot(nrow(codes) == 1)
  x$Value <- convert_values(x$Value, from = x$Units, to = codes$Unit)
  x$Units <- codes$Unit
  x
}

#' Standardises Values and Units
#'
#' Where possible standardises values and units
#'
#' @param x data.frame with columns Variable, Value and Units
#' @param messages flag indicating whether to print messages
#' @param parallel flag indicating whether to calculate limits by the by argument using the parallel backend provided by foreach
#' @examples
#' data(dummy)
#' standardise_values_units(dummy, messages = TRUE)
#' @export
standardise_values_units <- function (x, messages = getOption("wqbc.messages", default = TRUE),
                                parallel = getOption("wqbc.parallel", default = FALSE)) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(messages) && noNA(messages))
  assert_that(is.flag(parallel) && noNA(parallel))

  check_columns(x, c("Date", "Variable", "Value", "Units"))

  check_class_columns(x, list("Variable" = c("character", "factor"),
                              "Value" = "numeric",
                              "Units" = c("character", "factor")))

  if(!nrow(x)) return (x)

  x$Variable <- substitute_variables(x$Variable, messages = messages)
  x$Units <- substitute_units(x$Units, messages = messages)

  is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
  is.na(x$Units[!x$Units %in% get_units()]) <- TRUE

  x$Value <- replace_negative_values_with_na(x$Value, messages = messages)

  # need message here
  x <- dplyr::filter_(x, ~!is.na(Variable) & !is.na(Value) & is.na(Units))

  x <- plyr::ddply(x, .variables = "Variable", .fun = standardise_values_units_variable,
              .parallel = parallel, messages = messages)
}
