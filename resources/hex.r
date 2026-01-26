library(ggplot2)
library(hexSticker)

set.seed(2026)

dat <- data.frame(
  x = rnorm(200)
)

dat$y <- 0.8 * dat$x + rnorm(nrow(dat))

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = 3, alpha = 0.3, color = "white") +
  geom_smooth(method = "lm", se = FALSE, lwd = 1) +
  xlim(c(-3, 3)) +
  ylim(c(-3, 3)) +
  coord_fixed(ratio = 1) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  hexSticker::theme_transparent() +
  theme(
    axis.line = element_line(color = "white"),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


sticker(
  p,
  package = "ADCOM",
  p_size = 7,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.3,
  s_height = 1,
  h_fill = "#9F2B68",
  h_color = scales::alpha("white", 0),
  p_color = scales::alpha("white", 1),
  filename = "resources/hex.svg"
)
