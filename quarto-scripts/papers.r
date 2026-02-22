devtools::load_all()
library(RefManageR)

bib <- ReadBib("papers/papers.bib")
keys <- names(bib)
title <- sapply(bib, function(x) x$title)
title <- gsub("\\{|\\}|\n|", "", title)
title <- gsub("  ", " ", title)
file <- sapply(bib, function(x) x$file)
file <- sprintf("https://stat-teaching.github.io/adcom/papers/%s", file)

sprintf(
  "- key: '@%s'\n  title: '%s'\n  file: '[{{< fa file-pdf >}}](%s)'",
  keys,
  title,
  file
) |>
  cat(file = "papers/papers.yml", sep = "\n")
