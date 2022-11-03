library(rstan)
library(tidyverse)
library(here)
options(mc.cores = parallel::detectCores())

# data processing --------------------------------------------------------------

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))

# for now need to remove counties with all zero
dat_full <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  group_by(COUNTYCD) %>% 
  filter(!all(DRYBIO_AG_TPA_live_ADJ == 0)) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup()

dat_nz <- dat_full %>% 
  filter(DRYBIO_AG_TPA_live_ADJ > 0)


stan_list <- list(
  n = nrow(dat_nz),
  p = 2,
  rfid = dat_nz$group_id,
  j = length(unique(dat_nz$group_id)),
  x = model.matrix(~ tcc + tnt, dat_nz),
  y = dat_nz$DRYBIO_AG_TPA_live_ADJ
)


fit_gamma <- stan(file = "stan-models/zi-stan-separate-build/y_mod_gamma.stan",
                  data = stan_list,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)
