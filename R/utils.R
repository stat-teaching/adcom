wooclap <- function(x) {
  title <- ifelse(
    knitr::is_latex_output(),
    "\\large{Question Time!}",
    "Question Time"
  )
  cat(
    sprintf("<center> Question %s Time! {{< qrcode %s >}} </center>", title, x)
  )
}

pdf <- function(x) {
  if (knitr::is_latex_output()) {
    ghlink(x)
  } else {
    x
  }
}

ghlink <- function(x) {
  url <- system(
    "git config --get remote.origin.url | sed -E 's#.*/([^/]+/[^/.]+)(\\.git)?#\1#'",
    intern = TRUE
  )
  url <- gsub("git\\@github.com\\:", "", url)
  url <- xfun::sans_ext(url)
  sprintf("https://%s.github.io/%s/%s", dirname(url), basename(url), x)
}


mdag <- function(..., labels = NULL, focus = NULL, layout = "circle") {
  dots <- list(...)
  if (!is.null(labels)) {
    dots$labels <- labels
  }

  tdag <- do.call(ggdag::dagify, dots) |>
    ggdag::tidy_dagitty(layout = layout) |>
    ggdag::node_status()

  if (!is.null(labels)) {
    tdag$data$label <- ifelse(
      is.na(tdag$data$label),
      tdag$data$name,
      tdag$data$label
    )
  }

  if (!is.null(focus)) {
    tdag$data$status <- as.character(tdag$data$status)
    tdag$data$status[tdag$data$name %in% focus] <- "focus"
    tdag$data$status <- factor(tdag$data$status)
  }

  plt <- tdag |>
    ggplot2::ggplot(ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      color = status
    )) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point(size = 20) +
    ggdag::theme_dag() +

    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(
      values = c(
        "exposure" = "#0072B2",
        "outcome" = "firebrick",
        "latent" = "grey50",
        "focus" = "#009E73"
      ),
      na.value = "grey80"
    )

  if (is.null(labels)) {
    plt + ggdag::geom_dag_text(col = "white", size = 10)
  } else {
    plt +
      ggdag::geom_dag_label(
        ggplot2::aes(label = label),
        check_overlap = TRUE,
        col = "black"
      )
  }
}

reveal2pdf <- function(html) {
  pdf <- xfun::with_ext(html, "pdf")
  xaringan::decktape(html, pdf)
}


get_all_slides <- function() {
  list.files("_site/slides", pattern = "*.html", full.names = TRUE)
}

slides_db <- function() {
  slides <- get_all_slides()
  if (!file.exists(".db/slides.rds")) {
    db <- data.frame(
      file = slides,
      md5 = unname(tools::md5sum(slides))
    )
  } else {
    db <- readRDS(".db/slides.rds")
  }

  db$update <- FALSE
  new_slides <- setdiff(slides, db$file)
  if (length(new_slides) > 0) {
    new_slides <- data.frame(
      file = new_slides,
      md5 = unname(tools::md5sum(new_slides))
    )
    db <- rbind(db, new_slides)
  }
  db$update <- !tools::md5sum(slides) == db$md5
  saveRDS(db, ".db/slides.rds")
  readRDS(".db/slides.rds")
}


get_slide_files <- function() {
  files <- list.files(
    here::here("slides/files"),
    pattern = "*.pdf",
    full.names = TRUE
  )
  files <- lapply(files, function(x) {
    sprintf(
      "https://stat-teaching.github.io/adcom/%s",
      sub("^.*?slides/", "slides/", x)
    )
  })
  names(files) <- sapply(files, basename)
  files
}

sdag <- function(..., n = 100, standardized = TRUE, empirical = FALSE) {
  ffl <- list(...)
  ffl <- sapply(ffl, formula.tools:::as.character.formula)
  ffl <- paste(ffl, collapse = "\n")
  lavaan::simulateData(
    ffl,
    standardized = standardized,
    sample.nobs = n,
    empirical = empirical
  )
}

tab <- function(x, nbin = max(1, x, na.rm = TRUE)) {
  w <- tabulate(x, nbins = nbin)
  x <- sort(unique(x))
  list(x = x, w = w)
}

bullet_files <- function(dir) {
  fl <- list.files(dir, full.names = TRUE)
  sprintf("- [%s](%s)", basename(fl), fl) |>
    cat(sep = "\n")
}

mass_density_plot <- function(
  N,
  mean = 0,
  sd = 1,
  b = 10,
  xlab = NULL,
  hg = b / 2,
  max_digits = 4
) {
  xlab <- if (is.null(xlab)) "x" else xlab

  df <- data.frame(x = rnorm(N, mean, sd))
  brks <- seq(min(df$x), max(df$x), length.out = b + 1)
  binw <- diff(brks)[1]

  bins0 <- hist(df$x, breaks = brks, plot = FALSE)
  bins <- data.frame(
    mids = bins0$mids,
    density = bins0$density,
    count = bins0$counts
  )

  xlims <- range(brks)
  brks_lbl <- brks[seq(1, length(brks), by = 2)]

  # Pick digits based on bin width (smaller bin width -> more digits)
  digits <- max(0, min(max_digits, ceiling(-log10(binw))))
  acc <- 10^(-digits)

  lab_x <- scales::label_number(accuracy = acc, trim = TRUE)

  bins$lbl <- sprintf(
    "$\\frac{prob.\\ mass}{bin\\ width} = \\frac{%s / %s}{%.3f} = %.3f$",
    bins$count,
    N,
    binw,
    (bins$count / N) / binw
  )

  p_up <- ggplot(df, aes(x, seq_along(x))) +
    geom_point(alpha = 0.3, col = "dodgerblue") +
    geom_vline(xintercept = brks) +
    geom_text(
      data = bins,
      aes(x = mids, y = N / 2, label = count),
      angle = 90
    ) +
    scale_x_continuous(
      limits = xlims,
      breaks = brks_lbl,
      labels = lab_x,
      expand = c(0, 0)
    ) +
    xlab(xlab) +
    ylab(sprintf("# %s", xlab)) +
    coord_cartesian(clip = "off")

  p_down <- ggplot(df, aes(x, after_stat(density))) +
    geom_histogram(breaks = brks, fill = "dodgerblue", col = "black") +
    geom_point(data = bins, aes(mids, density), size = 3) +
    geom_point(
      data = bins[hg, ],
      aes(mids, density),
      col = "firebrick",
      size = 4
    ) +
    xlab(xlab) +
    ylab("Density") +
    scale_x_continuous(
      limits = xlims,
      breaks = brks_lbl,
      labels = lab_x,
      expand = c(0, 0)
    ) +
    annotate(
      "text",
      x = mean - 2 * sd,
      y = quantile(bins$density, 0.90),
      size = 4,
      label = latex2exp::TeX(bins$lbl[hg], output = "character"),
      parse = TRUE
    )

  p_up /
    p_down +
    patchwork::plot_layout(heights = c(0.3, 0.7), axes = "collect")
}


coin_table <- function(n, theta = NULL, table = TRUE) {
  space <- gtools::permutations(
    n = 2,
    r = n,
    v = c(0, 1),
    repeats.allowed = TRUE
  )
  S <- space
  S[S == 0] <- "⚪"
  S[S == 1] <- "🔵"
  D <- data.frame(S)
  colnames(D) <- sprintf("n%s", 1:ncol(D))
  D$E <- 1:nrow(D)
  D$k <- apply(space, 1, sum)

  if (!is.null(theta)) {
    p <- lapply(theta, function(p) p^(D$k) * (1 - p)^(n - D$k))
    names(p) <- sprintf("theta %s", theta)
    D <- cbind(D, p)
  }
  if (table) {
    tinytable::tt(D)
  } else {
    D$event <- apply(D[, sprintf("n%s", 1:ncol(S))], 1, paste, collapse = "")
    D
  }
}


# Funzione per disegnare l'albero
coin_tree <- function(n, p = 0.5) {
  plot(
    NULL,
    xlim = c(0, n + 0.5),
    ylim = c(-(2^n), 2^n),
    xlab = "",
    ylab = "",
    axes = FALSE,
    main = paste(n, "Lanci di Moneta")
  )

  # Funzione ricorsiva interna per i rami
  plot_branch <- function(x, y, step, n) {
    if (x < n) {
      # Calcoliamo le posizioni verticali dei nuovi nodi
      y_top <- y + 2^(n - x - 1)
      y_bottom <- y - 2^(n - x - 1)

      # Disegniamo i segmenti (i rami)
      segments(x, y, x + 1, y_top)
      segments(x, y, x + 1, y_bottom)

      # Scriviamo le etichette T e C
      text(x + 1, y_top, "T", pos = 3, font = 2)
      text(x + 1, y_bottom, "C", pos = 1, font = 2)

      # Continuiamo la ricorsione
      plot_branch(x + 1, y_top, step, n)
      plot_branch(x + 1, y_bottom, step, n)
    } else {
      # Siamo alla fine: scriviamo la probabilità finale
      prob_finale <- p^n
      text(
        x,
        y,
        paste0("p=", prob_finale),
        pos = 4,
        col = "firebrick",
        cex = 0.8
      )
    }
  }

  text(0, 0, "S", pos = 2)
  plot_branch(0, 0, 1, n)
}

exact_rnorm <- function(n, mean = 0, sd = 1, empirical = TRUE) {
  MASS::mvrnorm(n, mean, sd^2, empirical = empirical)[, 1]
}

rsum <- function(n, sum) {
  cuts <- runif(n - 1, min = 0, max = sum)
  sc <- sort(c(0, cuts, sum))
  diff(sc)
}

new_mean <- function(x, nm, n = 1) {
  nn <- length(x) + n
  xn <- (nm * nn) - sum(x)
  c(x, rsum(n, xn))
}

entropy <- function(x, probs = FALSE, relative = TRUE) {
  if (!probs) {
    p <- prop.table(table(x))
  } else {
    p <- x
  }
  E <- -sum(p * log(p))
  if (relative) {
    E <- E / log(length(p))
  }
  E[is.nan(E)] <- 0
  E
}

index_plot <- function(x, xlim = NULL) {
  xn <- deparse(substitute(x))
  dat <- data.frame(x, id = 1:length(x))
  dat$m <- mean(dat$x)
  dat$r <- dat$x - dat$m
  plot(
    id ~ x,
    data = dat,
    cex = 1.5,
    xlab = xn,
    xlim = xlim
  )
  abline(v = mean(dat$x), lwd = 2, col = "firebrick")
  segments(dat$x, dat$id, dat$m, dat$id, lty = "dotted")
}


ggnorm <- function(
  mean = 0,
  sd = 1,
  xlab = NULL,
  ylab = NULL,
  lwd = 0.5,
  lty = "solid"
) {
  xlab <- if (is.null(xlab)) "x"
  ylab <- if (is.null(ylab)) "Density"
  xlim <- c(
    mean - sd * 4,
    mean + sd * 4
  )
  ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = dnorm,
      args = list(
        mean = mean,
        sd = sd
      ),
      lwd = lwd,
      lty = lty
    ) +
    ggplot2::xlim(xlim) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(sprintf("mean = %s, sd = %s", mean, sd))
}

ggnorm_area <- function(mean = 0, sd = 1, area, fill = "black", alpha = 0.5) {
  xlim <- c(
    mean - sd * 4,
    mean + sd * 4
  )
  area[area == -Inf] <- xlim[1]
  area[area == Inf] <- xlim[2]
  ggplot2::stat_function(
    geom = "area",
    fun = dnorm,
    args = list(
      mean = mean,
      sd = sd
    ),
    xlim = area,
    fill = fill,
    alpha = alpha
  )
}
