

#Version 3
audit1 <- data.frame(list(ID = 1:6,
                          audit1_label = c('Never', 'Monthly or less', 
                                           '2 to 4 times a month', '2 to 3 times a week',
                                           '4 to 5 times a week', '6 or more times a week'),
                          audit1_value = c(0, 4.5 * 12,
                                           3 * 365 / 7, # the item caused most problems
                                           6 * 365 / 7,
                                           5 * 365 / 7, 7 * 365 / 7),
                          audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L)))
audit2 <- data.frame(list(ID = 1:7,
                          audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                                           "7 to 9", "10 to 12", "13 to 15", "16 or more"),
                          audit2_value = c(3.5, 5.5, 8,
                                           11, 12, 15, 18),
                          audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L)))


audit3 <- data.frame(list(ID = 1:5,
                          audit3_label = c("Never", "Less than monthly", "Monthly",
                                           "Weekly", "Daily or almost daily"),
                          audit3_value = c(0, 6, 15,
                                           1 * 365 / 7, 6.5 * 365 / 7),
                          audit3_score = 0:4))




proc.AUDIT <- function(X){
  X[,c("audit1_value", "audit1_score")] <- audit1[match(X$audit1_label,audit1$audit1_label), c("audit1_value", "audit1_score")]
  X[,c("audit2_value", "audit2_score")] <- audit2[match(X$audit2_label,audit2$audit2_label), c("audit2_value", "audit2_score")]
  X[,c("audit3_value", "audit3_score")] <- audit3[match(X$audit3_label,audit3$audit3_label), c("audit3_value", "audit3_score")]
  # X$audit2_label <- merge(X, audit2[,-1], all.x = T, by = "audit2_label")
  # X$audit3_label <- merge(X, audit3[,-1], all.x = T, by = "audit3_label")
  X$auditc_score <- X$audit1_score + ifelse(X$audit1_score == 0, 0, X$audit2_score + X$audit3_score )
  X$intake <- X$audit1_value * ifelse(X$audit1_value==0, NA, X$audit2_value )/52.1 
  bingeval <- 8
  X$qfv <- ifelse(X$audit1_score > 0,
                  ifelse(X$audit2_score >= 2,
                         ifelse(X$audit1_value >= X$audit3_value,
                                X$audit1_value * X$audit2_value  / 52.1,
                                X$audit3_value * X$audit2_value  / 52.1),
                         ifelse(X$audit1_value == X$audit3_value,
                                X$audit1_value * bingeval / 52.1,
                                ((X$audit1_value * X$audit2_value ) + (X$audit3_value * bingeval ) ) /52.1)),
                  0)
  return(X)
}

convpDrink <- function(prev_nondrinkers = NULL){
  cutoff = round(prev_nondrinkers, 2)
  return(
    data.frame(pct_all = c(0:(cutoff * 100),
                           seq((cutoff*100+1), 100,
                               length.out = (1 - cutoff) * 100)),
               pct_drinker = c(rep(NA, cutoff * 100 + 1),
                               seq(((cutoff*100+1)/100 - prev_nondrinkers)/(1-prev_nondrinkers), 1,
                                   length.out = (1 - cutoff) * 100 ))
    )
  )
}