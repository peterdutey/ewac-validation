
source("0 functions/functions.R")
source("0 functions/filepaths.R")
library(data.table)
library(haven)
ats <- haven::read_dta(file.path(atsfiles, "qfmodule.dta"))
ats <- ats[,listvar]

alctypes <- data.frame(list(alctype1 = c('Wine', 'Beer or lager', 
                                         'Spirits on their own (for example whisky, vodka)',
                                         'Cider', 'Alcopops (for example WKD, Smirnoff Ice)',
                                         'Mixed drinks (for example gin and tonic, whisky and coke)', 
                                         'Other'),
                            favdrink = c('Wine', 'Beer', 'Spirits alone',
                                         'Cider', 'Other', 'Mixed spirits', 'Other')))

ats$alctype1 <- as_factor(ats$alctype1)
ats <- merge(ats, alctypes, by = "alctype1", all.x = T, all.y = F)
ats$audit1_label <- as.character(as_factor(ats$audit1))
ats$audit2_label <- as.character(as_factor(ats$audit2))
ats$audit3_label <- as.character(as_factor(ats$audit3))
ats <- data.table(ats)
ats$sex <- as_factor(ats$sexz)
ats$sex <- relevel(ats$sex, "Women")
save(ats, file = file.path(atsfiles, "qfmodule.RData"))


#### Health Survey for England

hse <- foreign::read.dta("~/0 Datasets/HSE 2011/stata9_se/hse2011ai.dta", convert.factors = F)
names(hse) <- tolower(names(hse))
hse <- hse[hse$age > 17 & hse$sex ==2 ,]
hse$ageg <- cut(hse$age, breaks = c(17, 25, 35, 45, 55, 65, 75, 85, 105), include.lowest = F, right = F)
levels(hse$ageg) <- c("18-24 years","25-34 years","35-44 years","45-54 years",
                      "55-64 years","65-74 years","75-84 years","85 years and over")
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

hse <- hse[,c("hserial", "pserial", "hhsize", "psu", "wt_int", "age",  "wt_drink",
              "totalwu", "alcbase", "totalwug", "diaryrec", "ddunitwk", "ddunitwd",
              "weektot", "ddalclim", "sex", "tenureb", "gor1", "dnoft", "dnnow")]

save(hse, file =  file.path(atsfiles, "HSE.RData"))




# compare ATS, hse2011, hse2011 diaries


###

source("0 functions/functions.R")
source("0 functions/filepaths.R")
library(data.table)
library(haven)

load(file.path(atsfiles, "qfmodule.RData"))
ats <- merge(ats, audit1, "audit1_label", all.x = T, all.y = F)
ats <- merge(ats, audit2, "audit2_label", all.x = T, all.y = F)
ats <- merge(ats, audit3, "audit3_label", all.x = T, all.y = F)
ats <- proc.AUDIT(ats)
ats <- ats[audit1 %in% 2:6 & audit2 %in% 1:7,]


as_factor(ats$ta47_04)

var(ats[,.(qfv, gfmeanweekly)], na.rm =T)
library(scales)
plot(ats$qfv, ats$gfmeanweekly, pch =16, col = alpha(1, .2),
     ylab = "GF (units/week)", xlab = "EWAC (units/week)", main = NULL)
abline(0,1, col = 2, lwd =1.5)



plot(ats$qfv, ats$gfmeanweekly,  col = alpha(1, .5),
     xlim = c(0, 50), ylim = c(0, 50))

table(ats$qfv > 14, ats$gfmeanweekly >14)


m1 <- lm((intake-gfmeanweekly)^2~ as_factor(agez) + as_factor(qual) + as_factor(ta47_04),
         data = ats[ats$gfmeanweekly <150,])

m1 <- lm((qfv-gfmeanweekly)^2~ as_factor(agez) + as_factor(qual) + as_factor(ta47_04),
         data = ats[ats$gfmeanweekly <150,])


m1 <- lm((qfv-gfmeanweekly)^2~ 1, data = ats)
m1 <- lm((intake-gfmeanweekly)^2~ 1, data = ats)
summary(m1)
hist(residuals(m2), breaks = 200, xlim = c(-50, 50))
moments::skewness(residuals(m2))
moments::kurtosis(residuals(m2))
TA47_04. 

m2 <- lm(qfv-gfmeanweekly ~ as_factor(agez) + as_factor(sexz) + as_factor(favdrink), data = ats[gfmeanweekly <20,])
summary(m2)

library(mgcv)
m2 <- gam(qfv-gfmeanweekly ~  s(actage) + sex + favdrink, data = ats)
summary(m2)



m2 <- lm(qfv-gfmeanweekly ~ 1, data = ats)
summary(m2)
m2 <- lm(intake-gfmeanweekly ~ 1, data = ats)
summary(m2)



mhse <- gam((totalwu - weektot)~s(age)+factor(sex), data=hse)
summary(mhse)


summary(ats[,grep("gf", names(ats), value = T)])
# Removing never drinkers, 

table(as.factor(ats$auditriskzone0, ats$auditc))


calib <- ats[,.(audit1, audit2, audit3, agez, gfmeanweekly)]

m3 <- lm(log(gfmeanweekly + .5) ~  as_factor(audit1) + as_factor(audit2) -1,
         data = ats[gfmeanweekly <20,])
exp(summary(m3)$coef)



m <- glm(weektot~1, data = hse, family = Gamma)
library(MASS)

plot(density(hse$totalwu[which(hse$totalwu>0)]), col = 3, xlim = c(0, 150))
lines(density(hse$weektot[which(hse$weektot>0)]))
lines(density(ats$gfmeanweekly[which(ats$gfmeanweekly>0)]), col = 2)





plot(density(na.omit(hse$totalwu[which(hse$dnnow ==1)])), col = 3, 
     xlim = c(0, 50), main="audit-c>=1", ylim = c(0, .12))
lines(density(na.omit(ats$qfv[which(ats$auditc >=1)])), col = "grey75")
lines(density(na.omit(hse$weektot[which(hse$dnnow ==1)])))
lines(density(na.omit(ats$gfmeanweekly[which(ats$auditc >=1)])), col = 2)
legend("topright", 
       legend=c("HSE", "HSE diary", "ATS GF"), 
       lwd= 2,
       col = c(3, 1, 2))

library(fitdistrplus)
fitdist(na.omit(ats$gfmeanweekly[which(ats$auditc >=1 & ats$gfmeanweekly>0)]), distr = "gamma")


shapediary = 1.3401780
ratediary= 0.1080454

theta = 19.03
k = 0.69

curve(dpoisson(x, shape = shapediary, rate = ratediary), from=0, to =150)
curve(dpoisson(x, shape = k  , scale = theta*7/10), from=0, to =200, add = T, col = 2)
curve(dpoisson(x, shape = 0.78345929  , rate = 0.07667736), from=0, to =200, add = T, col = 3)

glm(ats$gfmeanweekly[which(ats$auditc >=1)]~1, family = poisson)
plot(density(na.omit(ats$gfmeanweekly[which(ats$auditc >=1)])), col = 3, 
     xlim = c(0, 50), main="audit-c>=1", ylim = c(0, .12))
curve(dpois(x, 9.954254), from=0, to =150, add = T)



library(fitdistrplus)
tlfb <- fitdist(hse$weektot[which(hse$weektot>0)], "gamma")




      