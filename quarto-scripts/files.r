devtools::load_all()
files <- list.files("slides/files", pattern = "*.pdf")

sprintf(
  "- title: '%s'\n  file: '[{{< fa file-pdf >}}](%s)'",
  basename(files),
  sprintf("https://stat-teaching.github.io/adcom/slides/files/%s", files)
) |>
  cat(file = "slides/files/files.yml", sep = "\n")
