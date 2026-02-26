if (Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == "0") {
  quit()
}

qmd <- list.files("slides", pattern = "*.qmd", full.names = TRUE)
html <- list.files("docs/slides", pattern = "*.html", full.names = TRUE)
qmd <- qmd[xfun::sans_ext(basename(qmd)) %in% xfun::sans_ext(basename(html))]

pdf <- list.files("docs/slides", pattern = ".pdf", full.names = TRUE)
pdf_pages <- sapply(qmd, function(x) {
  rmarkdown::yaml_front_matter(x)$`pdf-pages`
})

pdf_pages <- lapply(pdf_pages, function(x) if (is.null(x)) 0 else x)

for (i in 1:length(pdf)) {
  out <- sprintf("docs/slides/sharing/%s", basename(pdf[i]))
  if (pdf_pages[[i]] != 0) {
    pdftools::pdf_subset(
      pdf[i],
      1:pdf_pages[[i]],
      output = out
    )
  } else {
    fs::file_copy(pdf[i], out, overwrite = TRUE)
  }
}
