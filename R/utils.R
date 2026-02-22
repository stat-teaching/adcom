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

mdag <- function(...) {
  ggdag::dagify(
    ...
  ) |>
    ggdag::ggdag_status(node_size = 20, text_size = 10) +
    ggdag::theme_dag() +
    ggplot2::theme(legend.position = "none")
}

html <- "_site/slides/00-intro-corso.html"

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
