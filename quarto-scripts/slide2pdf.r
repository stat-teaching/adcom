if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == 0) {
  quit()
}

devtools::load_all()

slides <- slides_db()

for (i in 1:nrow(slides)) {
  if (slides$update[i]) {
    xaringan::decktape(
      slides$file[i],
      xfun::with_ext(slides$file[i], "pdf")
    )
    slides$update[i] <- FALSE
  }
}

saveRDS(slides, ".db/slides.rds")
