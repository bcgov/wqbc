theme_wqis <- function () {
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ggplot2::theme_bw()
}

theme_map <- function () {
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
