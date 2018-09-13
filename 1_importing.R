
source("0 functions/functions.R")
source("0 functions/filepaths.R")
library(data.table)
library(haven)
ats <- haven::read_dta(file.path(atsfiles, "qfmodule.dta"))
ats <- ats[,listvar]
ats$audit1_label <- as.character(as_factor(ats$audit1))
ats$audit2_label <- as.character(as_factor(ats$audit2))
ats$audit3_label <- as.character(as_factor(ats$audit3))

save(ats, file = file.path(atsfiles, "qfmodule.RData"))


###

source("../0 functions/functions.R")
source("../0 functions/filepaths.R")
library(data.table)
library(haven)


load(file.path(atsfiles, "qfmodule.RData"))
summary(ats[,grep("gf", names(ats), value = T)])
# Removing never drinkers, 
ats <- ats[audit1 %in% 2:6]

table(as.factor(ats$auditriskzone0, ats$auditc)

      