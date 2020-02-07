
source("01_src/functions.R")
source("01_src/filepaths.R")
source("01_src/audit_weights.R")


listvar <- c('xwave', 'A_weight0', 'weighttns', 'weighttns13', 'sttime', 'tatime', 'ststart', 
             'tastart', 'weekz', 'rectool', 'recz', 'st01', 'ta01', 'month', 'quarter', 
             'quarter2', 'qx', 'xyear', 'sexz', 'actage', 'agez', 'over35', 'sgz', 'randm', 
             'randm2', 'ab', 'sregz', 'gore', 'gor', 'nsec', 'qhhold', 'qwork', 'qemploy', 
             'q632e55', 'qstaff', 'qsuper', 'qprof', 'work', 'cie', 'mshop', 'super', 'wrkcie',
             'maritl', 'tenure', 'tennet', 'income', 'rel', 'cigsmok', 'smokstat',
             'income3', 'dethnin', 'ethnic', 'lstage', 'qual', 'audit1', 'audit2', 'audit3', 
             'auditc', 'auditce', 'fullaudit', 'fullaudite', 'auditriskzone', 'acmeanweekly',
             'acgroups', 'ta47_01_num', 'ta47_01_banded', 'ta47_02_num', 'ta47_02_banded', 
             'ta47_03_1', 'ta47_03_10', 'ta47_03_11', 'ta47_03_2', 'ta47_03_3', 'ta47_03_4', 
             'ta47_03_5', 'ta47_03_6', 'ta47_03_7', 'ta47_03_8', 'ta47_03_9', 'ta47_03_dk', 
             'ta47_03_ref', 'ta47_04', 'alctype1', 'alcmotiv', 'alccd1', 'alccutdown', 'alcatt', 
             'alcattnet', 'alcatt2', 'alcatt2n', 'tryalclyc', 'tryalclyc2', 'eq5df',
             'gf0102', 'gf0304', 'gf0507', 'gf0810', 'gf1115', 'gf1620', 'gf2130', 'gf3140', 
             'gf4150', 'gf5160', 'gfgroups', 'gfmax', 'gfmeanweekly', 'gfsumdays', 'gftotal')

library(haven)
library(lubridate)
ats <- read_dta(file.path(dir_ats, "qfmodule.dta"))
ats <- ats[, listvar]
ats$ageg <- cut(ats$actage, breaks = c(15, 17, 25, 35, 45, 55, 65, 75, 85, 105), 
                include.lowest = F, right = F)
ats$ageg7 <- cut(ats$actage, breaks = c(15, 25, 35, 45, 55, 65, 75, 105), 
                 include.lowest = F, right = F)
levels(ats$ageg) <- c("16-17 years", "18-24 years","25-34 years","35-44 years","45-54 years",
                      "55-64 years","65-74 years","75-84 years","85+ years")
levels(ats$ageg7) <- c("16-24 years","25-34 years","35-44 years","45-54 years",
                      "55-64 years","65-74 years","75+ years")
alctypes <- data.frame(list(
        alctype1 = c('Wine', 'Beer or lager', 
                     'Spirits on their own (for example whisky, vodka)',
                     'Cider', 'Alcopops (for example WKD, Smirnoff Ice)',
                     'Mixed drinks (for example gin and tonic, whisky and coke)', 
                     'Other'),
        favdrink = c('Wine', 'Beer', 'Spirits alone',
                     'Cider', 'Other', 'Mixed spirits', 'Other')
        ))

ats$alctype1 <- as_factor(ats$alctype1)
ats <- merge(ats, alctypes, by = "alctype1", all.x = T, all.y = F)
ats$ethnic <- as.character(as_factor(ats$ethnic))
ethnicgrp <- data.frame(list(ethnic = c('WHITE BRITISH', 'WHITE IRISH', 
                                        'WHITE GYPSY /TRAVELLER', 'WHITE OTHER', 
                                        'MIXED WHITE/BLACK CARIBBEAN', 
                                        'MIXED WHITE/BLACK AFRICAN', 
                                        'MIXED WHITE AND ASIAN', 'MIXED OTHER', 
                                        'ASIAN INDIAN', 'ASIAN PAKISTANI', 
                                        'ASIAN BANGLADESHI', 'ASIAN CHINESE', 
                                        'ASIAN OTHER', 'BLACK AFRICAN', 
                                        'BLACK CARIBBEAN', 'BLACK OTHER', 'ARAB', 
                                        'OTHER', "DON'T KNOW", 'REFUSED'),
                             ethgrp = c('White British', 'White Other',
                                        'White Other', 'White Other',
                                        'Mixed', 'Mixed', 'Mixed', 'Mixed',
                                        'Asian', 'Asian', 'Asian', 'Asian', 'Asian',
                                        'Black', 'Black', 'Black',
                                        'Other', 'Other', NA, NA)))
ats <- merge(ats, ethnicgrp, by = "ethnic", all.x =T)

ats <- ats %>% 
  mutate(ethgrp = factor(ethgrp, 
                         levels = c("White British", "White Other", "Mixed",
                                    "Asian", "Black", "Other"))) %>% 
  mutate(marital = as_factor(ats$maritl),
         rel = as_factor(rel),
         qual = as_factor(qual),
         smokstat = as_factor(smokstat)) %>% 
  mutate(marital = case_when(
    marital == "MARRIED - PARENT/GUARDIAN" ~ "Married",
    marital == "MARRIED - NOT PARENT/GUARDIAN" ~ "Married",
    marital == "SINGLE - PARENT/GUARDIAN" ~ "Single",
    marital == "SINGLE - NOT PARENT/GUARDIAN" ~ "Single",
    marital == "WID/DIV/SEP - PARENT/GUARDIAN" ~ "Separated/widowed",
    marital == "WID/DIV/SEP - NOT PARENT/GUARDIAN" ~ "Separated/widowed",
    TRUE ~ NA_character_
  )) %>% 
  mutate(highqual = case_when(
    qual == 'NO FORMAL QUALIFICATIONS' ~ "No qualification",
    qual %in% c('GCSE/O-LEVEL/CSE', 'VOCATIONAL QUALIFICATIONS (=NVQ1+2)', 
                'A-LEVEL OR EQUIVALENT (=NVQ3)') ~ "NVQ <= 3",
    qual %in% c('MASTERS/PHD OR EQUIVALENT', 
                "BACHELOR DEGREE OR EQUIVALENT (=NVQ4)") ~ "NVQ4+ (degree)",
    qual %in% c('OTHER', 'STILL STUDYING') ~ "Other",
    TRUE ~ NA_character_
  )) %>% 
  mutate(highqual = factor(highqual, 
                           levels = c("No qualification", "NVQ <= 3", 
                           "NVQ4+ (degree)", "Other")))

ats <- ats %>% 
  mutate(religion = case_when(
    rel %in% c("Baha'i", 'Buddhist', 'Hindu', 
               'Jain', 'Rastafarian', 'Sikh', 'Jewish',
               'Zoroastrian', 'Any other religion') ~'Any other religion',
    rel == "Refused" ~ NA_character_,
    rel == "Don't know" ~ NA_character_,
    TRUE ~ as.character(rel)
  )) %>% 
  mutate(religion = factor(religion, levels = c("No religion", "Christian", "Muslim", 
                                                "Any other religion"))) %>% 
  mutate(tryalclyc = factor(tryalclyc, 
                            labels = c("No attempt to cut down",
                                       "Serious attempt to cut down in last 12 months")),
         tryalclyc2 = factor(tryalclyc2, 
                            labels = c("No attempt to cut down",
                                       "Attempt to cut down in last 12 months")))

ats$sex <- as_factor(ats$sexz)
ats$sex <- relevel(ats$sex, "Women")

ats$date_wave <- as.Date("2014/03/01") %m+% months(ats$xwave - 90)

ats$audit1_label <- as.character(as_factor(ats$audit1))
ats$audit2_label <- as.character(as_factor(ats$audit2))
ats$audit3_label <- as.character(as_factor(ats$audit3))

ats$auditc_risk_level <- proc_AUDIT_risk(ats$auditc)

save(ats, file = file.path("02_data", "ATS.rda"))

rm(list = ls())