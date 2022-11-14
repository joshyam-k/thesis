library(tidyverse)
library(rstan)




stan_list_mod1 <- list(
  n = nrow(data_nz),
  p = 2,
  rfid = data_nz$group_id,
  j = length(unique(data_nz$group_id)),
  x = model.matrix(~ tcc + tnt, data_nz),
  y = data_nz$Y
)

stan_list_mod2 <- list(
  n = nrow(data),
  p = 2,
  rfid = data$group_id,
  j = length(unique(data$group_id)),
  x = model.matrix(~ tcc + tnt, data),
  z = as.integer(as.logical(data$DRYBIO_AG_TPA_live_ADJ))
)


fit_gamma <- stan(file = "stan-models/zi-stan-separate-build/y_mod_gamma.stan",
                  data = stan_list_mod1,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)


fit_p_mod <- stan(file = "stan-models/zi-stan-separate-build/p_mod2.stan",
                  data = stan_list_mod2,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)