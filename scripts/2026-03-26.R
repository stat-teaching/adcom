library(readxl)

# leggo i dati (attenzione a dove voi li avete salvati)
dat <- read_xlsx("data/dimarco2020.xlsx")

head(dat)

# correlazione
R <- cov(dat[, 10:15])

# boxplot condizionato
boxplot(burnout ~ education, data = dat)
