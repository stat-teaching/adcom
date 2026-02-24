plot_reg <- function(data, b0 = NULL, b1 = NULL, sep = 0, SST = TRUE){

  fit <- lm(y ~ x, data = data)

  if (is.null(b0)) {
    if (!is.null(b1)) {
      b0 <- mean(data$y)
    } else {
      b0 <- coef(fit)[1]
    }
  }

  if (is.null(b1)) b1 <- coef(fit)[2]

  data$mu  <- with(data, b0 + b1 * x)
  data$mup <- with(data, b0 + b1 * (x + sep))
  data$mum <- with(data, b0 + b1 * (x - sep))
  data$ybar <- mean(data$y)
  data$ri <- data$y - data$mu
  SS <- sum(data$ri^2)

  ggplot(data, aes(x, y)) +

    # Regression line
    geom_abline(
      intercept = b0,
      slope = b1
    ) +

    # Mean line
    geom_hline(
      aes(yintercept = ybar),
      color = "firebrick",
      linetype = "dashed"
    ) +

    # Residual variation (SSE)
    geom_segment(
      aes(
        x = x + sep, xend = x + sep,
        y = y, yend = mup,
        color = "Residual (SSE)",
        linetype = "Residual (SSE)"
      )
    ) +

    # Explained variation (SSR)
    geom_segment(
      aes(
        x = x - sep, xend = x - sep,
        y = mum, yend = ybar,
        color = "Explained (SSR)",
        linetype = "Explained (SSR)"
      )
    ) + {
      if(SST){
        # Total variation (SST)
    geom_segment(
      aes(
        xend = x,
        y = y, yend = ybar,
        color = "Total (SST)",
        linetype = "Total (SST)"
      ),
      linewidth = 1
    )
      }
    } +
    # Points
    geom_point(size = 2) +

    # Legend definitions
    scale_color_manual(
      name = "Sum of Squares",
      values = c(
        "Residual (SSE)"  = "black",
        "Explained (SSR)" = "firebrick",
        "Total (SST)"     = "darkgray"
      )
    ) +
    scale_linetype_manual(
      name = "Sum of Squares",
      values = c(
        "Residual (SSE)"  = "dotted",
        "Explained (SSR)" = "solid",
        "Total (SST)"     = "solid"
      )
    ) +

    # Make legend lines clearer
    guides(
      color = guide_legend(override.aes = list(linewidth = 1.2))
    ) +

    ggtitle(sprintf("Residual SSE = %.2f", SS)) +

    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom"
    )
}


# Data
#set.seed(1)
x <- seq(0, 10, length.out = 30)
y <- 2.5 * x + 1 + rnorm(length(x), 0, 3)
df <- data.frame(x, y)
data <- df

plot_res <- function(data, show.data = FALSE){
  fit <- lm(y ~ x, data = data)
  data$mu  <- with(data, b0 + b1 * x)
  data$mup <- with(data, b0 + b1 * (x + sep))
  data$mum <- with(data, b0 + b1 * (x - sep))
  data$ybar <- mean(data$y)
  data$ri <- data$y - data$mu
  SS <- sum(data$ri^2)
  data$id <- 1:nrow(data)

  if (is.null(b0)) {
    if (!is.null(b1)) {
      b0 <- mean(data$y)
    } else {
      b0 <- coef(fit)[1]
    }
  }

  if (is.null(b1)) b1 <- coef(fit)[2]

  ggplot(data, aes(x = id, y = ri)) +
    geom_point()

}

plot_res(df, show.data = TRUE)
