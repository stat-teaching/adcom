dat <- readxl::read_xlsx("data-raw/Q01-adcom.xlsx")
dat <- dat[, -1]
dat <- dat[sample(1:nrow(dat), size = 400, replace = TRUE), ]
dat <- cbind(id = 1:nrow(dat), dat)
names(dat) <- c(
  "id",
  "altezza",
  "scarpe",
  "coffee",
  "sex",
  "residence",
  "analisi"
)
dat$altezza <- ifelse(dat$altezza < 100, dat$altezza * 100, dat$altezza)
dat <- dat[dat$coffee < 20, ]
rownames(dat) <- NULL

saveRDS(dat, "data/adcom.rds")
writexl::write_xlsx(dat, "data/adcom.xlsx")
