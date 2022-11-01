library(rstan)
# run parallel processing
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(here)

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
  
stan_list_mod1 <- list(
  n = nrow(dat_nz),
  p = 2,
  rfid = dat_nz$group_id,
  j = length(unique(dat_nz$group_id)),
  x = model.matrix(~ tcc + tnt, dat_nz),
  y = dat_nz$DRYBIO_AG_TPA_live_ADJ
)

stan_list_mod2 <- list(
  n = nrow(dat_full),
  p = 2,
  rfid = dat_full$group_id,
  j = length(unique(dat_full$group_id)),
  x = model.matrix(~ tcc + tnt, dat_full),
  z = as.integer(as.logical(dat_full$DRYBIO_AG_TPA_live_ADJ))
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


mean(ext_y_mod$beta[,3])

lme4::lmer(DRYBIO_AG_TPA_live_ADJ ~ tcc + tnt + (1 | group_id), dat)



  




