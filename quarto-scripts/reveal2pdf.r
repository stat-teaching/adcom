if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == "0") {
  quit()
}

fs::dir_create("docs/slides/sharing")

qmd <- list.files("slides", pattern = "*.qmd", full.names = TRUE)
html <- list.files("docs/slides", pattern = "*.html", full.names = TRUE)
qmd <- qmd[xfun::sans_ext(basename(qmd)) %in% xfun::sans_ext(basename(html))]

for (i in 1:length(html)) {
  system(sprintf("deck2pdf %s %s", html[i], xfun::with_ext(html[i], "pdf")))
}
