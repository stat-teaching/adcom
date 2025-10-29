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
