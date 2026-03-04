mtheme <- function(size = 20) {
  ggplot2::theme_minimal(base_size = size) +
    ggplot2::theme(legend.position = "bottom")
}
