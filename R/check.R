check_valid_expression <- function(x) {
  parse(text = x)
  invisible(NULL)
}

check_codes <- function(x) {
  chk::check_data(x,
                  list(Variable = "",
                       Code = "",
                       Units = lookup_units(),
                       EC_Code = c(1L, NA)),
                  key = "Variable")
  chk::check_key(x, "Code")
  invisible(NULL)
}

check_limits <- function(x) {
  chk::check_data(x,
                  list(Variable = lookup_variables(),
                       Use = lookup_use(),
                       Term = c("Short", "Long", "Long"),
                       Condition = c("", NA),
                       UpperLimit = "",
                       Units = lookup_units(),
                       Statistic = c("mean", "median", "max")),
                  key = c("Variable", "Use", "Term", "Condition"))

  check_valid_expression(x$Condition)
  check_valid_expression(x$UpperLimit)
  invisible(NULL)
}
