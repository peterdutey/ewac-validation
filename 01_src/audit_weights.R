


# Item interval midpoint --------------------------------------------------

audit_weights_midpoints <- list(
  audit1 = data.frame(list(
    audit1_label = c("Never", "Monthly or less", "2 to 4 times a month",
                     "2 to 3 times a week", "4 or more times a week",
                     "4 to 5 times a week", "6 or more times a week"),
    audit1_value = c(0, 12, 3 * 12,
                     2.5 * 365 / 7, 5 * 365 / 7,
                     4.5 * 365 / 7, 6.5 * 365 / 7),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 or more",
                     "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(1.5, 3.5, 5.5, 
                     8, 12,
                     11, 14, 16),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0, 6, 12,
                     1 * 365 / 7, 6.5 * 365 / 7),
    audit3_score = 0:4))
)


# Version 3 ---------------------------------------------------------------
# Used in ABH prototype last version
audit_weights_version3 <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0, 4.5 * 12,
                     3 * 365 / 7, # the item caused most problems
                     6 * 365 / 7,
                     5 * 365 / 7, 7 * 365 / 7),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(3.5, 5.5, 8,
                     11, 12, 15, 18),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0, 6, 15,
                     1 * 365 / 7, 6.5 * 365 / 7),
    audit3_score = 0:4))
  
)


# Version 4 ---------------------------------------------------------------
# Attempt to re-estimate
audit_weights_version4 <- list(
  audit1 = data.frame(list(
    audit1_label = c('Never', 'Monthly or less', 
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0, .3465425, .9434469 , 2.216192 ,4.199273, 6.053639)*365 / 7,
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.290841, 4.045133, 5.81638, 7.71108, 9.645919, 11.45553, 15.12373),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0, 6, 15,
                     1 * 365 / 7, 6.5 * 365 / 7),
    audit3_score = 0:4))
)



# ATS version -------------------------------------------------------------
# previously estimated directly from ATS
audit_weights_ATS <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     1.178, 
                     2.087, 
                     4.632, 
                     8.563, 
                     11.986),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(3.5, 5.5, 8,
                     11, 12, 15, 18),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0, 6, 15,
                     1 * 365 / 7, 6.5 * 365 / 7),
    audit3_score = 0:4))
  
)



# STAN --------------------------------------------------------------------


audit_weights_STAN <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     11.79, 
                     35.05, 
                     104.48, 
                     183.91, 
                     299.83),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.02, 
                     3.66, 
                     5.16, 
                     8.42, 
                     11.15, 
                     14.01, 
                     19.96),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(8.63, 
                     15.28, 
                     36.04, 
                     63.62, 
                     5 * 365 / 7),
    audit3_score = 0:4))
  
)

, 
, 
, 


