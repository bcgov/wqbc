punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

remove_columns_from_x_in_y <- function (x, y) {

  colnames <- colnames(x)[colnames(x) %in% colnames(y)]
  if(length(colnames) >= 1) {
    message("Removed columns ", punctuate_strings(colnames, "and"), " from x")
  }
  x[,!colnames(x) %in% colnames(y), drop = FALSE]
}
