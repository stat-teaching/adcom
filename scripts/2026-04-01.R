# puliamo l'ambiente
rm(list = ls())

# importiamo i dati
dat <- readRDS("data/adcom.rds")

# ora immaginiamo che dat sia la popolazione e non un campione
# e da dat prendiamo un campione di osservazioni. Quindi dat
# è la "verità" solitamente incognita che vogliamo stimare mentre
# il campione è quello che solitamente possiamo osservare

# numerosità del nostro campione
n <- 15

# possiamo usare la funzione sample() per estrarre delle righe casuali
# dal nostro dataframe

# ad esempio, questo estrae 5 numeri da 1 a 100 dove ognuno ha la stessa
# probabilità di essere estratto. Ogni volta che lanciamo la funzione il
# risultato è diverso

sample(1:100, 5)
sample(1:100, 5)
sample(1:100, 5)

# ora per estrarre un campione usiamo questa logica. il nostro dataset ha
# N righe. quindi se estriamo a caso n numeri da 1 a N e teniamo quelle righe
# stiamo campionando casualmente.

# popolazione
N <- nrow(dat)

idx <- sample(1:N, n, replace = FALSE)

# queste sono le righe che teniamo dalla prima estrazione. possiamo usare idx
# per selezionare le righe dal dataframe

# questo è il nostro campione ed è quello che succede normalmente.

dat2 <- dat[idx, ]

# facciamoci una funzioncina che esegue questa operazione

campiona <- function(n, data){
    idx <- sample(1:nrow(data), n, replace = FALSE)
    data[idx, ]
}

campiona(10, dat)
campiona(10, dat)
campiona(10, dat)

# ora immaginiamo di voler stimare l'altezza media della nostra
# popolazione. il parametro di solito è incognito ma in questa simulazione noi lo
# sappiamo, chiamiamolo mu.

mu <- mean(dat$altezza)
mu

# ora proviamo ad estrarre un campione di numerosità n e vediamo la nostra stima

camp <- campiona(n, dat)
xbar <- mean(camp$altezza)
xbar

# come vedete non è esattamente lo stesso valore stiamo facendo una stima e per
# definizione non è possibile ottenere lo stesso valore. ora proviamo a ripetere
# questa stima

camp <- campiona(n, dat)
xbar <- mean(camp$altezza)
xbar

# come vedete ancora diversa, certe volte di più e certe volte di meno. proviamo
# a ripeterla molte volte. usiamo la funzione replicate()

xsamp <- replicate(1000, {
    camp <- campiona(n, dat)
    mean(camp$coffee)
})

# abbiamo ripetuto il campionamento ed il calcolo della media per 1000 volte e
# e quindi abbiamo ottenuto 1000 medie. come potete intuire queste 1000 medie
# non sono tutte uguali, quindi proviamo a vedere questa distribuzione

hist(xsamp, breaks = 15)

# vediamo che le stime non si distribuiscono casualmente
# - la distribuzione è tendenzialmente normale
# - ha una certa media e una certa varianza
# - valori estremi sono poco frequenti

# vediamo ora dove si colloca la media "vera" ovvero quella della popolazione

abline(v = mu, lwd = 2, col = "firebrick")

# come vedete, la media (incognita) della popolazione è molto simile alla media
# di tutte le stime (della media). aggiungiamo anche quella

abline(v = mean(xsamp), lwd = 2, col = "dodgerblue")

# mettiamo tutto dentro una funzione e vediamo cosa succede.

sampd <- function(n, x, data){
    replicate(5000, {
        camp <- campiona(n, data)
        mean(camp[[x]])
    })
}

# ora possiamo fare tutto in modo più semplice e cambiando i parametri
# vediamo cosa succede facendo la stessa operazione con n diversi

nn <- c(5, 15, 30, 50)
res <- lapply(nn, sampd, "altezza", dat)

# calcoliamo medie e deviazioni standard per ogni n

sapply(res, mean)
sapply(res, sd)

# come vedete, la media sembra essere praticamente sempre identica. la deviazione
# standard invece è chiaramente diversa, in modo decrescente. più aumentiamo n
# più le stime sono concentrate attorno alla media. vediamolo anche graficamente

par(mfrow = c(2, 2))

xlim <- c(min(res[[1]]), max(res[[1]]))

hist(res[[1]], xlim = xlim, main = nn[1])
hist(res[[2]], xlim = xlim, main = nn[2])
hist(res[[3]], xlim = xlim, main = nn[3])
hist(res[[4]], xlim = xlim, main = nn[4])

# quali conclusioni possiamo trarre?
# 
# all'aumentare del campione (casuale) la nostra stima
# diventa meno variabile. Comunque abbiamo delle variazioni (errore)


# questo è l'unico caso (impossibile) dove la stima non ha variabilità.
sampd(N, "coffee", dat)

# anche un'osservazione in meno crea dell'errore di stima
sampd(N - 1, "coffee", dat)

# abbiamo trovato "empiricamente" una quantità fondamentale in
# inferenza ovvero quella di errore di stima. solitamente questa
# viene chiamata errore standard. nel caso della media la formula
# è semplice
# 
# se = sd / sqrt(n)
# dove sd è la deviazione standard della popolazione e n è la numerosità campionaria. Chiaramente in ottica applicata si usa
# la deviazione standard del campione come stima di quella della
# popolazione. ma noi abbiamo anche quella vera.

n <- 15
s <- sd(dat$altezza)

se <- (s / sqrt(n))
se

# vediamo quella "empirica"

res <- sampd(n, "altezza", dat)
sd(res)

# come vedete l'errore standard che stimiamo con la formula è esattamente quello
# che stimiamo empiricamente. è importante notare che questa stima "empirica" non
# è possibile farla nella realtà. ma assumendo che il campionamento sia casuale
# vediamo che la media delle medie è uguale alla media vera e l'errore di stima
# è possibile quantificarlo.

# vediamo cosa succede con un campionamento non casuale. Proviamo a creare dei
# campioni dove la % di maschi è maggiore di quello che dovrebbe essere.

campiona_bias <- function(n, data){
    p <- ifelse(data$sex == "Maschile", 0.9, 0.1)
    idx <- sample(1:nrow(data), n, replace = FALSE, prob = p)
    data[idx, ]
}

dcamp <- replicate(1000, {
    camp <- campiona_bias(20, dat)
    mean(camp$altezza)
})

mu
mean(dcamp)

par(mfrow = c(1, 1))
hist(dcamp)
abline(v = mean(dcamp), lwd = 3, col = "dodgerblue")
abline(v = mu, lwd = 3, col = "firebrick")

# abbiamo introdotto un bias sistematico, non è casuale. questo è quello
# che succede quando stimiamo qualcosa campionado in modo non casuale
