# explained and unexplained variance

N <- 200
x <- c("a" = 0, "b" = 1)
x <- rep(x, each = N / 2)
y <- 10 + 20 * x + rnorm(N, 0, 10)

dat <- data.frame(
  y,
  x = names(x)
)

# grafico 1, distribuzione grezza di y

ggplot(dat, aes(x = y)) +
  geom_density() +
  geom_rug()

# grafico 2, distribuzione grezza di y con colore in base a x

ggplot(dat, aes(x = y)) +
  geom_density() +
  geom_rug(aes(color = x)) +
  theme(legend.position = "bottom")

# grafico 3, medie di y e y ~ x

ggplot(dat, aes(x = y)) +
  geom_density() +
  geom_rug(aes(color = x)) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(dat$y), lty = "dotted") +
  annotate(geom = "label", x = mean(dat$y), y = 0.01, label = "mean(y)") +
  geom_vline(xintercept = mean(dat$y[dat$x == "a"]), color = "firebrick") +
  annotate(
    geom = "label",
    x = mean(dat$y[dat$x == "a"]),
    y = 0.005,
    label = "mean(y|x = a)"
  ) +
  geom_vline(xintercept = mean(dat$y[dat$x == "b"]), color = "dodgerblue") +
  annotate(
    geom = "label",
    x = mean(dat$y[dat$x == "b"]),
    y = 0.005,
    label = "mean(y|x = b)"
  )

# Understanding Quantiles ------------------------------------------------

library(tidyverse)
library(patchwork)

y <- rnorm(1e3, 100, 20)
dat <- data.frame(y)

qq <- data.frame(
  q = c(0.25, 0.5, 0.75)
)

dat <- rbind(dat, data.frame(y = c(200, 200, 250)))
dat$new <- ifelse(dat$y < 200, 0, 1)

q <- c(0.25, 0.5, 0.75)

q_old <- data.frame(
  q = q,
  y = quantile(dat$y[dat$new == 0], q),
  data = "old"
)

q_new <- data.frame(
  q = q,
  y = quantile(dat$y, q),
  data = "new"
)

qq <- rbind(q_old, q_new)

p1 <- ggplot(dat[dat$new == 0, ], aes(x = y)) +
  geom_histogram(bins = 50) +
  geom_vline(data = qq[qq$data == "old", ], aes(xintercept = y)) +
  xlim(c(10, 300))

p2 <- ggplot(dat, aes(x = y)) +
  geom_histogram(bins = 50) +
  geom_vline(data = qq[qq$data == "new", ], aes(xintercept = y)) +
  xlim(c(10, 300))

p1 / p2

N <- 1e6
x <- rep(0:1, each = N / 2)
z <- 0.4 * x + rnorm(N)
y <- 0.2 * x + 0.1 * z + rnorm(N)

fit <- lm(y ~ x)
summary(fit)


x <- 1:10
x <- x - mean(x)

(t(x) %*% x) / (length(x) - 1)
var(x)
