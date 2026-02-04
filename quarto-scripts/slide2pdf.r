if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == 0) {
  quit()
}

library(pagedown)
library(renderthis)


slides <- list.files("_site/slides", pattern = "*.html", full.names = TRUE)

chrome_print("_site/slides/00-intro-corso.html")
