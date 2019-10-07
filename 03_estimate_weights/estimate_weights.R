# Estimating 

rm(list = ls())

# Loading data
source("01_src/functions.R")
source("01_src/audit_weights.R")

options(mc.cores = parallel::detectCores())
library(tidyverse)
library(rstan)

load(file.path("02_data", "ATS.rda"))
ats <- ats %>% 
  # filter(actage >= 18) %>%
  # filter(sex == "Women") %>%
  filter(sex == "Men") %>%
  filter(audit1 > 1 & audit1 < 7) %>% 
  filter(!audit2_label %in% c("Refused", "Don't know")) %>% 
  filter(!audit3_label %in% c("Refused", "Don't know")) %>% 
  mutate(audit1_label = factor(audit1_label, 
                               levels = c('Never', 'Monthly or less',
                                          '2 to 4 times a month', '2 to 3 times a week',
                                          '4 to 5 times a week', '6 or more times a week')),
         audit2_label = factor(audit2_label,
                               levels = c("1 to 2", "3 to 4", "5 to 6",
                                          "7 to 9", "10 to 12", "13 to 15", "16 or more")), 
         audit3_label = factor(audit3_label, 
                               levels = c("Never", "Less than monthly", "Monthly",
                                          "Weekly", "Daily or almost daily")))

# ats$ewac_mp <- proc_EWAC(
#   ats[,c("audit1_label", "audit2_label", "audit3_label")],
#   audit_coef = audit_weights_midpoints, method = "qfv")

ats_subset <- ats %>% 
  transmute(GF = gfmeanweekly,
            audit1_ = as.factor(audit1),
            audit2_ = as.factor(audit2),
            audit3_ = as.factor(audit3)) %>% 
  model.frame(data = ., formula = GF ~ audit1_ + audit2_ + audit3_)

stan_input <- data.frame(
  model.matrix(~ audit1_ + audit2_ + audit3_, ats_subset, 
               contrasts = list(
                 audit1_ = contrasts(ats_subset$audit1_, contrasts = F),
                 audit2_ = contrasts(ats_subset$audit2_, contrasts = F),
                 audit3_ = contrasts(ats_subset$audit3_, contrasts = F)
               ))
)[,-1]

stan_input$GF <- ats_subset$GF
stan_input <- na.omit(stan_input)

stan_input <- as.list(stan_input)
stan_input$N <- length(stan_input$audit1_2)
rm(ats_subset)


# STAN model fitting ------------------------------------------------------

mod1_fit <- stan(
  file = "03_estimate_weights/stan_model.stan",  # Stan program
  data = stan_input,    # named list of data
  chains = 3,             # number of Markov chains
  warmup = 3000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 3,              # number of cores (could use one per chain)
  # control = list(adapt_delta = .95, max_treedepth = 12),#  refresh = 200,
  init = list(
    list(sigma = 10, bingehyper = 1, 
         F2hyper = 0.182, F3hyper = 0.19, F4hyper = 0.4, F5hyper = 0.75, F6hyper = 0.75, 
         Q1hyper = 0.333, Q2hyper = 0.5, Q3hyper = 0.5, Q4hyper = 0.5, Q5hyper = 0.75, Q6hyper = 0.727, Q7hyper = 0.474, 
         V1hyper = 0.01, V2hyper = 0.5, V3hyper = 0.4, V4hyper = 0.5, V5hyper = 0.5),
    list(sigma = 12, bingehyper = 0.1, 
         F2hyper = 0.229, F3hyper = 0.267, F4hyper = 0.667, F5hyper = 0.833, F6hyper = 0.833, 
         Q1hyper = 0.444, Q2hyper = 0.583, Q3hyper = 0.583, Q4hyper = 0.611, Q5hyper = 0.917, Q6hyper = 0.788, Q7hyper = 0.579, 
         V1hyper = 0.1, V2hyper = 0.75, V3hyper = 0.6, V4hyper = 0.75, V5hyper = 0.75), 
    list(sigma = 8, bingehyper = 2,
         F2hyper = 0.156, F3hyper = 0.15, F4hyper = 0.608, F5hyper = 0.706, F6hyper = 0.706,
         Q1hyper = 0.275, Q2hyper = 0.456, Q3hyper = 0.456, Q4hyper = 0.441, Q5hyper = 0.662, Q6hyper = 0.695, Q7hyper = 0.368,
         V1hyper = 0.5, V2hyper = 0.375, V3hyper = 0.5, V4hyper = 0.45, V5hyper = 0.625)
  ))



# Extract results ---------------------------------------------------------

new_results_dir <- file.path("03_estimate_weights", "results_5000_2019-10-07_men")
if (!dir.exists(new_results_dir)) { 
  dir.create(new_results_dir)
}

results <- summary(mod1_fit, pars = c('F2', 'F3', 'F4', 'F5', 'F6', 'Q1', 'Q2', 'Q3', 
                                      'Q4', 'Q5', 'Q6', 'Q7', 'V1', 'V2', 'V3', 'V4', 
                                      'binge', 'sigma'))$summary
write.csv(results, file = file.path(new_results_dir, "coefficients.csv"))

d1 <- plot(mod1_fit, plotfun = "trace", pars = c('F2', 'F3', 'F4', 'F5', 'F6'))
d2 <- plot(mod1_fit, plotfun = "trace", pars = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7'))
d3 <- plot(mod1_fit, plotfun = "trace", pars = c('V1', 'V2', 'V3', 'V4', 'binge', 'sigma'))

save_trace_plots <- function(x, file){
  pdf(file, width = 8, height = 4)
  plot(x)
  dev.off()
  x
}
save_trace_plots(d1, file.path(new_results_dir, "traces_AUDIT1.pdf"))
save_trace_plots(d2, file.path(new_results_dir, "traces_AUDIT2.pdf"))
save_trace_plots(d3, file.path(new_results_dir, "traces_AUDIT3.pdf"))

#rm(list= ls())


