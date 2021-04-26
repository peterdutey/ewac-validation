


# Item interval midpoint --------------------------------------------------

audit_weights_midpoints <- list(
  audit1 = data.frame(list(
    audit1_label = c("Never", "Monthly or less", "2 to 4 times a month",
                     "2 to 3 times a week", "4 or more times a week",
                     "4 to 5 times a week", "6 or more times a week"),
    audit1_value = c(0, 12 / (365/7), 3 * 12 / (365/7),
                     2.5, 5,
                     4.5, 6.5),
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
    audit3_value = c(0, 6 / (365/7), 12 / (365/7),
                     1, 6.5),
    audit3_score = 0:4)),
  
  binge_value = 6
  
)


# Version 3 ---------------------------------------------------------------
# Used in ABH prototype last version
audit_weights_version3 <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0, 4.5 * 12 / (365/7),
                     3, # the item caused most problems
                     6, 5, 7),
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
    audit3_value = c(0, 6 / (365/7), 15 / (365/7),
                     1, 6.5),
    audit3_score = 0:4)),
  
  binge_value = 6
  
  
)


# Version 4 ---------------------------------------------------------------
# Attempt to re-estimate
audit_weights_version4 <- list(
  audit1 = data.frame(list(
    audit1_label = c('Never', 'Monthly or less', 
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0, .3465425, .9434469 , 2.216192 ,4.199273, 6.053639),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.290841, 4.045133, 5.81638, 7.71108, 9.645919, 11.45553, 15.12373),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0, 6 / (365/7), 15 / (365/7),
                     1, 6.5),
    audit3_score = 0:4)),
  
  binge_value = 6
  
)


# STAN --------------------------------------------------------------------


audit_weights_STAN <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     0.211,
                     0.579,
                     1.645,
                     3.314,
                     5.475),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.132,
                     4.354,
                     5.603,
                     6.950,
                     10.184,
                     10.859,
                     15.972),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),

  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0.129,
                     0.343,
                     0.645,
                     1.402,
                     5),
    audit3_score = 0:4)),

  binge_value = 5.703
)

audit_weights_STAN_women <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     0.188,
                     0.550,
                     1.636,
                     3.415,
                     5.138),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.305, 
                     4.111, 
                     5.54, 
                     7.067, 
                     9.733, 
                     11.076, 
                     16.369),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0.106,
                     0.270,
                     0.567,
                     1.234,
                     5),
    audit3_score = 0:4)),
  
  binge_value = 5.374
  
)

audit_weights_STAN_men <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     0.181,
                     0.571,
                     1.589,
                     3.265,
                     5.193),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
  
  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.404, 
                     4.366, 
                     5.974, 
                     6.887, 
                     9.754, 
                     9.701, 
                     17.990),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
  
  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0.157,
                     0.395,
                     0.696,
                     1.383,
                     5),
    audit3_score = 0:4)),
  
  binge_value = 6.059
  
)


