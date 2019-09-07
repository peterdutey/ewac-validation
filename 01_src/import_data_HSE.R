source("01_src/functions.R")
source("01_src/filepaths.R")

library(foreign)

hse <- read.dta(dir_hse, convert.factors = F)
names(hse) <- tolower(names(hse))
hse <- hse[ hse$age > 15 ,]
hse$ageg <- cut(hse$age, breaks = c(15, 17, 25, 35, 45, 55, 65, 75, 85, 105), include.lowest = F, right = F)
levels(hse$ageg) <- c("16-17 years", "18-24 years","25-34 years","35-44 years","45-54 years",
                      "55-64 years","65-74 years","75-84 years","85+ years")
hse$totalwu[hse$totalwu < 0] <- NA
hse$dnoft[hse$dnoft < 0] <- NA
hse$dnoft <- factor(hse$dnoft, 
                    labels = names(attributes(hse)$label.table$DNOFT[attributes(hse)$label.table$DNOFT > 0]))

hse$dnany[hse$dnany==2] <- "Never-never"
hse$dnany[hse$dnany==1] <- "Drinker (occasional)"
hse$dnany[hse$dnnow == 1] <- "Drinker"

hse$dnany[hse$dnany < 0] <- NA
hse$weektot[hse$weektot < 0 ] <- NA
hse$ddunitwk[hse$ddunitwk <0 ] <- NA
hse$ddunitwd[hse$ddunitwd <0 ] <- NA

hse$dnoft <- factor(hse$dnoft, labels = names(attributes(hse)$label.table$DNOFT[attributes(hse)$label.table$DNOFT > 0]))
hse$dnnow[hse$dnnow < 0] <- NA

hse <- hse[,c("hserial", "pserial", "hhsize", "psu", "wt_int", "age", "ageg", "wt_drink",
              "totalwu", "alcbase", "totalwug", "diaryrec", "ddunitwk", "ddunitwd",
              "weektot", "ddalclim", "sex", "tenureb", "gor1", "dnoft", "dnnow")]

save(hse, file = file.path("02_data", "HSE.rda"))


