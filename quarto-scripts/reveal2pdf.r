if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == "0") {
  quit()
}

html <- list.files("docs/slides", pattern = "*.html", full.names = TRUE)

for (i in 1:length(html)) {
  system(sprintf("deck2pdf %s %s", html[i], xfun::with_ext(html[i], "pdf")))
}
