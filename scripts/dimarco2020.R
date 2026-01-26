# Di Marco et al. (2020) Dataset -----------------------------------------

# This article provides a dataset examining how volunteers’ psychosocial resources
# (including professional quality of life, sense of community, and trauma experiences)
# relate to their overall quality of life. The dataset supports research into the interplay
# between volunteers’ psychological resources and well‑being outcomes, facilitating secondary
# analysis by researchers.
#
# reference https://doi.org/10.1016/j.dib.2020.105522

dat_orig <- readxl::read_xls("data-raw/dimarco2020.xls", sheet = 2)

recode <- function(x, table) {
    table[x]
}

dat <- dat_orig

# removing single items
start <- which(names(dat) == "age")
dat <- dat[, start:ncol(dat)]

dat <- dat[complete.cases(dat), ]
names(dat) <- tolower(names(dat))

dat$age2 <- recode(
    dat$age2,
    c(
        "1" = "<30",
        "2" = "31-40",
        "3" = "41-50",
        "4" = "51-60",
        "5" = ">60"
    )
)

names(dat)[names(dat) == "age2"] <- "age_cat"

dat$period2 <- recode(
    dat$period2,
    c(
        "1" = "<5",
        "2" = "6-10",
        "3" = "11-15",
        "4" = "16-20",
        "5" = "21-30",
        "6" = ">31"
    )
)

names(dat)[names(dat) == "period2"] <- "period_cat"

dat$education <- recode(
    dat$education - 1,
    c(
        "2" = "sec_school",
        "3" = "high_school",
        "4" = "degree"
    )
)

dat$residence <- recode(
    dat$residence,
    c(
        "1" = "north",
        "2" = "central",
        "3" = "south"
    )
)

dat$gender <- ifelse(dat$gender == 1, "male", "female")

names(dat)[names(dat) == "pse"] <- "perc_self_efficacy"
names(dat)[names(dat) == "pce"] <- "perc_collective_efficacy"

dat$id <- 1:nrow(dat)
dat <- dat[, c("id", names(dat)[names(dat) != "id"])]

saveRDS(dat, "data/dimarco2020.rds")
writexl::write_xlsx(dat, "data/dimarco2020.xlsx")
