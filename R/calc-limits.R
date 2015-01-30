# join_codes <- function (x) {
#   x <- dplyr::rename_(x, "..Units" = "Units")
#   x$Variable <- as.character(x$Variable)
#   x <- dplyr::left_join(x, wqbc_codes(), by = "Variable")
#   stopifnot(!any(is.na(x$Units)))
#   x$Value <- convert_values(x$Value, from = x$..Units, to = x$Units)
#   x$..Units <- NULL
#   x
# }
#
# average_daily_values_date_variable <- function (x) {
#   txt <- paste0("x$Value <- ", x$Average, "(x$Value)")
#   eval(parse(text = txt))
#   x[1,,drop = FALSE]
# }
#
# average_daily_values_date <- function (x) {
#   if(anyDuplicated(x$Variable))
#     x <- plyr::ddply(x, "Variable", average_daily_values_date_variable)
#   x
# }
#
# average_daily_values <- function (x) {
#   if(anyDuplicated(dplyr::select_(x, ~Date, ~Variable)))
#     x <- plyr::ddply(x, "Date", average_daily_values_date)
#   x
# }
#
# average_monthly_values_variable <- function (x) {
#   x$Weeks <- length(unique(lubridate::week(x$Date)))
#   x$Values <- nrow(x)
#   average_daily_values_date_variable(x)
# }
#
# average_monthly_values <- function (x) {
#   x <- plyr::ddply(x, "Variable", average_monthly_values_variable)
#   x
# }
#
# join_limits <- function (x) {
#   x$..ID <- 1:nrow(x)
#   x <- dplyr::left_join(x, wqbc_limits(), by = c("Variable", "Units"))
#   x
# }
#
# get_code_values <- function (x) {
#   code_value <-  as.list(x$Value)
#   names(code_value) <- x$Code
#   code_value[!duplicated(names(code_value))]
# }
#
# test_condition <- function (x, cv) {
#   if(is.na(x))
#     return (TRUE)
#   x <- try(eval(parse(text = x), envir = cv), silent = TRUE)
#   if(class(x) != "logical")
#     return (FALSE)
#   return (x)
# }
#
# calc_limit <- function (x, cv) {
#   x <- try(eval(parse(text = as.character(x)), envir = cv), silent = TRUE)
#   if(class(x) != "numeric")
#     return (NA)
#   return (x)
# }
#
# calc_limits_by_period <- function (x) {
#   cv <- get_code_values(x)
#   x$Condition <- vapply(x$Condition, FUN = test_condition,
#                         FUN.VALUE = logical(1), cv = cv)
#
#   x <- x[x$Condition,,drop = FALSE]
#   x$Condition <- NULL
#
#   x$UpperLimit <- vapply(x$UpperLimit, FUN = calc_limit,
#                          FUN.VALUE = numeric(1), cv = cv)
#
#   x[!is.na(x$UpperLimit),,drop = FALSE]
# }
#
# calc_limits_by_day <- function (x) {
#   x <- dplyr::filter_(x, ~!is.na(Term) & Term == "Short")
#   x <- plyr::ddply(x, "Date", calc_limits_by_period)
#   stopifnot(!anyDuplicated(x$..ID))
#   x$Term <- factor("Short", levels = c("Short", "Long"))
#   x
# }
#
# calc_limits_by_30day <- function (x) {
#   x <- dplyr::filter_(x, ~!is.na(Term) & Term == "Short")
#   x$Year <- lubridate::year(x$Date)
#   x$Month <- lubridate::month(x$Date)
#
#   x <- plyr::ddply(x, c("Year", "Month"), average_monthly_values)
#   x <- plyr::ddply(x, c("Year", "Month"), calc_limits_by_period)
#   x <- dplyr::filter_(x, ~Values >= 5 & Weeks >= 3)
#
#   if(!nrow(x))
#     return (x)
#
#   x$Date <- as.Date(paste(x$Year, x$Month, "01", sep = "-"))
#
#   stopifnot(!anyDuplicated(x$..ID))
#
#   x <- dplyr::select_(x, ~-..ID, ~-Code, ~-Average, ~-Year, ~-Month, ~-Values, ~-Weeks)
#   x
# }
#
# calc_limits_by <- function (x, term) {
#   x <- join_codes(x)
#   x <- average_daily_values(x)
#   x <- join_limits(x)
#
#   if(term == "long") {
#     x <- calc_limits_by_30day(x)
#   } else {
#     x <- calc_limits_by_day(x)
#   }
#   x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~Units, ~Term)
#   x
# }
#
# empty_limits <- function (x) {
#   x <- cbind(x, data.frame(Term = character(), UpperLimit = numeric()))
#   x <- dplyr::select_(x, ~Date, ~Variable, ~Value, ~UpperLimit, ~Units)
# }
#
# #' Calculates Water Quality limits
# #'
# #' Calculates the approved upper water quality thresholds for
# #' British Columbia.
# #'
# #' @inheritParams calc_wqis
# #' @param term A string indicating whether to calculate long-term
# #' (the default) or short-term (\code{term = short"}) limits.
# #' @param max_cv A number indicating the maximum permitted replicate coefficient
# #' of variation for replicates.
# #' @examples
# #' data(fraser)
# #' fraser <- calc_limits(rbind(fraser[1:100,],fraser[1:100,]), term = "short",
# #' messages = TRUE)
# #' @export
# calc_limits <- function (x, by = NULL, term = "long",
#                          messages = getOption("wqbc.messages", default = TRUE),
#                          parallel = getOption("wqbc.parallel", default = FALSE)) {
#
#   assert_that(is.data.frame(x))
#   assert_that(is.null(by) || (is.character(by) && noNA(by)))
#   assert_that(is.string(term))
#   assert_that(is.flag(messages) && noNA(messages))
#   assert_that(is.flag(parallel) && noNA(parallel))
#
#   term <- tolower(term)
#   if(!term %in% c("long", "short")) stop("term must be \"long\" or \"short\"")
#
#   x <- standardize_wqdata(x, messages = message)
#   x <- standardize_wqdata(x, messages = message)
#
#   x$Variable <- substitute_variables(x$Variable, messages = messages)
#   x$Units <- substitute_units(x$Units, messages = messages)
#
#   is.na(x$Variable[!x$Variable %in% get_variables()]) <- TRUE
#   is.na(x$Units[!x$Units %in% get_units()]) <- TRUE
#
#   x$Value <- replace_negative_values_with_na(x$Value, messages = messages)
#
#   x <- delete_rows_with_missing_values(x, messages = messages)
#
#   check_rows(x)
#
#   cv <- calc_rcvs(x, messages = TRUE)
#   if(messages) message("The maximum replicate coefficient of variation is ",
#                        max(cv$CV, na.rm = TRUE))
#
#   if(is.null(by)) return(calc_limits_by(x, term = term))
#
#   plyr::ddply(x, .variables = by, .fun = calc_limits_by, .parallel = parallel, term = term)
# }
