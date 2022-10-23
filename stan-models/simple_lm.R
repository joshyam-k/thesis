library(rstan)
# run parallel processing
options(mc.cores = parallel::detectCores())
library(tidyverse)

dat <- 

stan_list <- list(
  n = nrow(dat),
  p = 3,
  y = dat$DRYBIO_AG_TPA_live_ADJ,
  x = model.matrix(~ tcc + tnt, dat)
)


