ggbar <- function(x) {
  data.frame(x) |>
    ggplot(aes(x = x)) +
    geom_bar()
}

rm_title_x <- function(x) {
  theme(axis.title.x = element_blank())
}

gghist <- function(x, bins = 30) {
  data.frame(x) |>
    ggplot(aes(x = x)) +
    geom_histogram(bins = 30)
}
