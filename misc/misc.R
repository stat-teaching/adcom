library(ggplot2)
library(plotly)

theme_set(theme_bw(15))

n <- 1e3
z <- rnorm(n)

x <- 0.5 * z + rnorm(n)
y <- 0.2 * x + 0.5 * z + rnorm(n)

dat <- data.frame(x, y, z)

# 2-d, 3 variables

ggplot(dat, aes(x = x, y = y, color = z)) +
  geom_point() +
  scale_color_viridis_c() +
  xlim(c(-5, 5)) +
  ylim(c(-5, 5)) +
  coord_fixed(ratio = 1)

plotly::plot_ly(dat, x = ~x, y = ~y, z = ~z)


n <- 1000
true_ability <- rnorm(n, 50, 10)
noise_1 <- rnorm(n, 0, 10)
noise_2 <- rnorm(n, 0, 10)
midterm <- true_ability + noise_1
final <- true_ability + noise_2
exams <- data.frame(midterm, final)

fit_1 <- lm(final ~ midterm, data = exams)
plot(midterm, final, xlab = "Midterm exam score", ylab = "Final exam score")
abline(coef(fit_1))

fit <- lm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris)

X <- model.matrix(fit)


N <- 100

dep <- 1:20
p_acc <- plogis(qlogis(0.2) + 0.15 * (dep - 1))
p_app_women <- plogis(qlogis(0.8) - 0.15 * (dep - 1))
p_app_man <- 1 - p_app_women

accepted <- N * p_acc
n_women <- accepted * p_app_women
n_man <- accepted * p_app_man

dat <- data.frame(
  department = toupper(letters[dep]),
  n_women = round(n_women),
  n_man = round(n_man),
  n_accepted = round(accepted)
)

dat$p_accepted <- dat$n_accepted / 100

ggplot(dat, aes(x = p_accepted, y = department)) +
  geom_point(aes(size = n_accepted, color = n_women / n_accepted)) +
  scale_size_area(max_size = 10, guide = "none") +
  scale_color_viridis_c()


ggplot(dat, aes(x = p_accepted, y = department, fill = n_women / n_accepted)) +
  geom_col()

dat |>
  summarise(
    tot = sum(n_accepted),
    n_man = sum(n_man),
    n_women = sum(n_women)
  ) |>
  mutate(p_man = n_man / tot, p_women = n_women / tot)


dat |>
  mutate(p_women = n_women / n_accepted, p_man = n_man / n_accepted)


pval <- function(n, d) {
  se <- sqrt(1 / n + 1 / n)
  t <- d / se
  (1 - pnorm(abs(t))) * 2
}

# questo mostra che il pvalue è funzione di molte cose, alcune delle quali non rispondono direttamente alla domande di quanto è grande o rilevante quell'effetto

par(mfrow = c(1, 2))

d <- 0.3
n <- seq(2, 200, 1)

plot(n, pval(n, d), type = "l", main = "d = 0.3", ylab = "p value")
abline(h = 0.05)

d <- seq(0.01, 1, 0.01)
n <- 30

plot(d, pval(n, d), type = "l", main = "n = 30", ylab = "p value")
abline(h = 0.05)


find_n <- function(d, target = 0.05) {
  uniroot(
    f = function(n) pval(n, d) - target,
    interval = c(2, 1e6) # lower bound must be ≥2 for 2 groups
  )$root
}

dd <- expand.grid(
  d = seq(0.1, 1, 0.01),
  p = c(0.01, 0.05, 0.1)
)

dd$n <- mapply(function(d, p) find_n(d, p), dd$d, dd$p)

library(ggplot2)
library(tidyverse)

dd |>
  mutate(pval = factor(p)) |>
  ggplot(aes(x = n, y = d, color = pval)) +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom") +
  xlab("Sample Size") +
  ylab("Cohen's d") +
  ylim(c(0, 1)) +
  labs(color = "p value") +
  geom_hline(yintercept = 0.2, lty = "dotted") +
  scale_x_log10(n.breaks = 10)

d <- 0.3
n <- 30

get_inference <- function(n, d, alpha = 0.05) {
  # sensitivity
  se <- sqrt(1 / n + 1 / n)
  zc <- abs(qnorm(alpha / 2))
  z <- d / se
  tpr <- 1 - pnorm(zc - z) + pnorm(-zc - z)
  # specificity
  tnr <- 1 - alpha
  data.frame(tpr = tpr, tnr = tnr)
}

alpha <- seq(0.0001, 0.999, length.out = 100)
ii <- get_inference(10, 0.5, alpha)
plot(1 - ii$tnr, ii$tpr, type = "l")

alpha <- 0.05
trued <- 0.3
n <- c(10, 50, 100, 200)
se <- sqrt(1 / n + 1 / n)
zc <- abs(qnorm(alpha / 2))

expand.grid(
  d = seq(-0.5, 1, length.out = 100),
  n = n
) |>
  mutate(
    y = dnorm(d, trued, sqrt(1 / n + 1 / n)),
    p = pval(n, d),
    se = 1 / n + 1 / n,
    dc = tc * se,
    n = factor(n, levels = unique(n), labels = paste("n =", unique(n)))
  ) |>
  ggplot(aes(x = d, y = y)) +
  geom_area(aes(fill = p)) +
  scale_fill_viridis_c(begin = 0, end = 1) +
  geom_vline(aes(xintercept = dc)) +
  geom_vline(aes(xintercept = trued), lty = "dotted", lwd = 1) +
  theme_bw(base_size = 20) +
  xlab("Cohen's d") +
  facet_wrap(~n) +
  ggtitle(paste("true d = ", trued)) +
  geom_label(aes(x = dc, y = 0), label = "dcrit")
