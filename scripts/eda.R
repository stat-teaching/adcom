# 18 Marzo 2026

# Carico dimarco2020 -----------------------------------------------------

## qui dovete mettere il percorso dove è localizzato il file

dat <- readxl::read_xlsx("data/dimarco2020.xlsx")

## istogramma, di default

hist(dat$perc_collective_efficacy)

## istogramma con bin più stretti (e quindi maggiore numero di bin)
## più preciso avendo più intervalli

hist(dat$perc_collective_efficacy, breaks = 10)

# centrare e standardizzare ---------------------------------------------

## mettiamo dentro oggetto x per comodità

x <- dat$perc_collective_efficacy

xc <- x - mean(x) # centrare sulla media
xz <- (x - mean(x)) / sd(x) # standardizzare

## media 0, sd la stessa di x
mean(xc)
sd(xc)

## media = 0, sd = 1 per definizione

mean(xz)
sd(xz)

## in alternativa per centrare e standardizzare
scale(x, center = TRUE, scale = TRUE)

## solo centrare
scale(x, center = TRUE, scale = FALSE)
