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
