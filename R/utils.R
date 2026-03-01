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
