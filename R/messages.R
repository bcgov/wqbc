messages_match_substitution <- function (x, y, txt = "substitute") {
  x <- as.character(x)
  y <- as.character(y)
  d <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  d <- unique(d)
  d <- dplyr::arrange_(d, ~x)
  b <- !is.na(d$x) & !is.na(d$y) & d$x != d$y
  if(any(b)) {
    db <- d[b,,drop = FALSE]
    message(capitalize(txt), "d ",
            punctuate_strings(paste(db$x, "with", db$y), "and"), ".")
  }
  b <- !is.na(d$x) & is.na(d$y)
  if(any(b)) {
    db <- d[b,,drop = FALSE]
    message("Failed to ", txt, " ",
            punctuate_strings(db$x, "and"), ".")
  }
  NULL
}
