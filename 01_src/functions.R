
# proc.AUDIT <- function(X){
#   X[,c("audit1_value", "audit1_score")] <- audit1[match(X$audit1_label,audit1$audit1_label), c("audit1_value", "audit1_score")]
#   X[,c("audit2_value", "audit2_score")] <- audit2[match(X$audit2_label,audit2$audit2_label), c("audit2_value", "audit2_score")]
#   X[,c("audit3_value", "audit3_score")] <- audit3[match(X$audit3_label,audit3$audit3_label), c("audit3_value", "audit3_score")]
#   # X$audit2_label <- merge(X, audit2[,-1], all.x = T, by = "audit2_label")
#   # X$audit3_label <- merge(X, audit3[,-1], all.x = T, by = "audit3_label")
#   X$auditc_score <- X$audit1_score + ifelse(X$audit1_score == 0, 0, X$audit2_score + X$audit3_score )
#   X$intake <- X$audit1_value * ifelse(X$audit1_value==0, NA, X$audit2_value )/52.1 
#   bingeval <- 8
#   X$qfv <- ifelse(X$audit1_score > 0,
#                   ifelse(X$audit2_score >= 2,
#                          ifelse(X$audit1_value >= X$audit3_value,
#                                 X$audit1_value * X$audit2_value  / 52.1,
#                                 X$audit3_value * X$audit2_value  / 52.1),
#                          ifelse(X$audit1_value == X$audit3_value,
#                                 X$audit1_value * bingeval / 52.1,
#                                 ((X$audit1_value * X$audit2_value ) + (X$audit3_value * bingeval ) ) /52.1)),
#                   0)
#   return(X)
# }

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

tbrounding <- function(x, places = 1){
  return(format(round(x, places), nsmall = places))
}


keyABH <- list(space="right", cex = 1.5,
               lines=list(col=c("red", "red", "black", "orange"),
                          lty=c(1, 3, 1, 1), lwd=c( 1, 1, 2, 2)),
               text=list(c("HSE 2011", "HSE diary 2011",
                           "ATS GF", "EWAC")))

ecdfxlab=list(label="Average units/week", cex=1.5)
ecdfylab=list(label="Empirical cumulative density", cex=1.5)



proc_AUDIT_score <- function(X, audit_coef, zero.if.A1.Never = F){
  # Compute AUDIT-C score
  # X:                 data.frame containing AUDIT1, AUDIT2, and AUDIT3 columns in order 
  # audit_coef:        list of coefficients to apply to AUDIT response items (see audit_weights.R)
  # zero.if.A1.Never:  whether AUDIT-1 = 'Never' is treated as AUDIT-C = 0. Default is FALSE.
  
  X[,c("audit1_score")] <- audit_coef$audit1[match(X[,1], audit_coef$audit1$audit1_label), c("audit1_score")]
  X[,c("audit2_score")] <- audit_coef$audit2[match(X[,2], audit_coef$audit2$audit2_label), c("audit2_score")]
  X[,c("audit3_score")] <- audit_coef$audit3[match(X[,3], audit_coef$audit3$audit3_label), c("audit3_score")]
  
  if(zero.if.A1.Never){
    X$audit1_score + ifelse(X$audit1_score == 0, 0, X$audit2_score + X$audit3_score )
  } else {
    X$audit1_score + X$audit2_score + X$audit3_score 
  }
}

proc_AUDIT_risk <- function(X){
  # Compute Public Health England alcohol risk levels
  # X: vector of AUDIT-C scores
  cut(X, breaks = c(0, 5, 8, 13), right = F,
      labels = c("Low", "Increasing", "High"))
}


proc_EWAC <- function(X, audit_coef, method = "qfv"){
  # Compute the EWAC
  # X:                 data.frame containing AUDIT1, AUDIT2, and AUDIT3 columns in order 
  # audit_coef:        list of coefficients to apply to AUDIT response items (see audit_weights.R)
  # method:            character string indicating the estimation method: 
  #                      - "qfv" for the quantity-frequency-variably method
  #                      - "qf" for the simple quantity-frequency method.
  # binge.val:         number of UK units assumed for a bringe drinking session. Default is 8.
  
  X[,c("audit1_value")] <- audit_coef$audit1[match(X[,1], audit_coef$audit1$audit1_label), c("audit1_value")]
  X[,c("audit2_value")] <- audit_coef$audit2[match(X[,2], audit_coef$audit2$audit2_label), c("audit2_value")]
  X[,c("audit3_value")] <- audit_coef$audit3[match(X[,3], audit_coef$audit3$audit3_label), c("audit3_value")]
  binge.val <- audit_coef$binge_value
  if (method == "qf"){
    
    ( X$audit1_value * ifelse(X$audit1_value==0, NA, 
                              X$audit2_value) ) /52.1 
    
  } else if (method == "qv"){
    
    ( X$audit2_value * X$audit3_value ) / 52.1 
    
  } else if (method == "qfv") {

    ( (X$audit1_value * X$audit2_value ) + 
       (X$audit3_value * binge.val ) ) / 52.1
    
  } else if (method == "qb") {
    
    ( X$audit3_value * binge.val ) / 52.1
    
  } else if (method == "ewac") {
    
    ifelse(X$audit2_value >= binge.val,
           ifelse(X$audit1_value >= X$audit3_value,
                  #QF:
                  X$audit1_value * X$audit2_value  / 52.1,
                  #QV:
                  X$audit3_value * X$audit2_value  / 52.1),
           ifelse(X$audit1_value >= X$audit3_value,
                  #binge x F
                  X$audit1_value * binge.val / 52.1, 
                  #QF + binge x V
                  ((X$audit1_value * X$audit2_value ) + (X$audit3_value * binge.val ) ) /52.1))
    
    
  } else {
    
    stop("Invalid method input.")
    
  }
  
}
