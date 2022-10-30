library(rstan)
# run parallel processing
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(here)

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))

dat <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  mutate(group_id = group_indices(., COUNTYCD))

set.seed(22)

stan_list_mod1 <- list(
  n = nrow(dat),
  p = 3,
  rfid = dat$group_id,
  j = length(unique(dat$group_id)),
  x = model.matrix(~ tcc + tnt, dat),
  y = dat$DRYBIO_AG_TPA_live_ADJ
)

stan_list_mod2 <- list(
  n = nrow(dat),
  p = 3,
  rfid = dat$group_id,
  j = length(unique(dat$group_id)),
  x = model.matrix(~ tcc + tnt, dat),
  z = as.integer(as.logical(dat$DRYBIO_AG_TPA_live_ADJ))
)

fit_y_mod <- stan(file = "stan-models/zi-stan-separate-build/y_mod1.stan",
               data = stan_list_mod1,
               cores = parallel::detectCores(),
               iter = 10000,
               chains = 4)


fit_p_mod <- stan(file = "stan-models/zi-stan-separate-build/p_mod2.stan",
                  data = stan_list_mod2,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)


ext_y_mod <- rstan::extract(fit_y_mod)
ext_p_mod <- rstan::extract(fit_p_mod)

## predictions






  




