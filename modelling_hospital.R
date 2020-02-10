require(ggplot2)
require(pscl)
require(MASS)
require(boot)


# Loading data
source("../01_src/functions.R")
# source("../01_src/filepaths.R")
source("../01_src/audit_weights.R")
 
library(tidyverse)
library(knitr)
library(captioner)
library(kableExtra)
library(pROC)
library(MASS)
library(stargazer)
 

# Hospital data

load("../02_data/hospital.rda")
vname_audit_baseline <- c("audit1_bline", "audit2_bline", "audit3_bline")
vname_audit_fu <- c("audit1_fu", "audit2_fu", "audit3_fu")
qdata <- records$main 

# Compute AUDIT-C scores
qdata$auditc_score <- proc_AUDIT_score(
  qdata[,vname_audit_baseline], audit_weights_version3)
qdata$auditc_risk <- proc_AUDIT_risk(qdata$auditc_score)

# Compute EWAC
qdata$ewac_mp <- proc_EWAC(qdata[,vname_audit_baseline],
                           audit_weights_midpoints, method = "qfv")
qdata$ewac_stan <- proc_EWAC(qdata[,vname_audit_baseline],
                             audit_weights_STAN, method = "qfv" )
tdata_b <- records$tlfb %>%
  filter(encounter == "Baseline" &
           pid %in% unique(qdata$pid) &
           days_since <= 28)
tdata_b <- merge(tdata_b, qdata, by = "pid", all.x = T) %>% 
  filter(!is.na(ewac_stan))

hosp_miss <- tdata_b %>% 
  group_by(pid) %>% 
  summarise(n_missing = sum(is.na(alcunits)))

tdata_b$ewac_stan_log <- log(tdata_b$ewac_stan/7)
tdata_b$ewac_mp_log <- log(tdata_b$ewac_mp/7)

mhosp <- MASS::glm.nb(round(alcunits) ~ ewac_stan_log + q_admin_mode +
                        week_since + tlfb_weekday, data = tdata_b)

adjustment <- data.frame(list(
  week_since = c("1-7 days", "8-14 days", "15-21 days", "22-28 days"),
  adj_factor = c(1, 1/exp(coefficients(mhosp))[grep("^week_", names(coefficients(mhosp)))])
))

tlfb_measured <- tdata_b %>% 
  filter(!is.na(alcunits) & !is.na(ewac_stan)) %>% 
  dplyr::select(pid, q_admin_mode, ewac_stan, alcunits)

tlfb_imputed <- tdata_b %>% 
  filter(is.na(alcunits) & !is.na(ewac_stan)) 
tlfb_imputed$alcunits <-  predict(mhosp, tlfb_imputed, type = "response")

tlfb_person <- dplyr::select(tlfb_imputed, pid, q_admin_mode, ewac_stan, alcunits) %>% 
  bind_rows(., tlfb_measured) %>% 
  merge(., adjustment, all.x = T) %>% 
  group_by(pid, q_admin_mode, ewac_stan) %>% 
  summarise(tlfb_units = sum(alcunits)/4,
            tlfb_units_recall_adjusted = sum(alcunits * adj_factor)/4)

tlfb_descriptives <- list(
  MD = mean(tlfb_person$ewac_stan - tlfb_person$tlfb_units),
  MD.p = t.test(tlfb_person$ewac_stan - tlfb_person$tlfb_units)$p.value,
  RMSD = sqrt(mean((tlfb_person$ewac_stan - tlfb_person$tlfb_units)^2, na.rm = T)),
  MSD = mean((tlfb_person$ewac_stan - tlfb_person$tlfb_units)^2, na.rm = T),
  MSD.ME = 1.96 * sqrt(var((tlfb_person$ewac_stan - tlfb_person$tlfb_units)^2) / (nrow(tlfb_person) - 1)),
  RMSD.p = chisq.test.variance( (tlfb_person$ewac_stan - tlfb_person$tlfb_units)^2,
                                sigma = 4)$p.value
)


mhosp <- MASS::glm.nb(round(alcunits) ~ ewac_stan_log + q_admin_mode +
                        week_since + tlfb_weekday, data = tdata_b)
m1 <- zeroinfl(round(alcunits) ~ offset(ewac_stan_log) + q_admin_mode +
                 week_since + tlfb_weekday, data = tdata_b, dist = "negbin", link = "log")
m1 <- zeroinfl(round(alcunits) ~ offset(ewac_stan_log) + q_admin_mode +
                 week_since + tlfb_weekday, data = tdata_b, dist = "poisson", link = "log")


m1 <- hurdle(round(alcunits) ~ offset(ewac_stan_log) + q_admin_mode +
               week_since + tlfb_weekday, data = tdata_b, zero = "geometric")
summary(m1)



# 
# To impute those, as well as study the association with the EWAC and the Extended AUDIT-C's mode of administration, a negative binomial model is estimated (`r table_nums("tab_hosp_validity_reg", display="cite")`). This model suggests that the EWAC underestimates alcohol consumption; after accounting for weekday and recall error increasing with time, TLFB daily records are on average 4.0 times [95% CI: 3.5; 4.5] greater than the EWAC. Self-administration of the Extended AUDIT-C could enhance this pattern of underestimation: coefficients suggest that reports of daily alcohol consumed are an additional 7.6% [-0.8; 17%] higher than the self-administered EWAC, compared to the researcher-administered EWAC. Statistical evidence is weak to reject the null hypothesis ($p=0.074$). Recall bias is examined using 3 dummy variables indicating how many weeks have passed since the daily record's date. While previous research tends to indicate increasing bias as time passes, no evidence of this pattern is found in the dataset. On average, TLFB reports at 8 to 14 days and at 15 to 21 days respectively amount to 71% [52; 97%] and 66% [48; 89%] of reports at 1 to 7 days before interview. No such evidence of bias is detected beyond 21 days.
# 
# `r table_nums("tab_hosp_validity_reg")`
# 
# ```{r results='asis'}
# stargazer(mhosp, type = ifelse(knit_output_pdf, "latex", "html"),ci = rep(T, 2),
#           digits = 2, digits.extra	= 2, initial.zero = T, style = "qje",
#           single.row = T, intercept.bottom = F, align=TRUE, no.space= T)
# ```
# 
# 
# 
# 
# The model is used to impute `r nrow(tlfb_imputed)` missing daily alcohol consumption records in total for `r length(unique(tlfb_imputed$pid))` participants. Non-missing records are not adjusted for recall bias in the absence of precise coefficient estimates.  
