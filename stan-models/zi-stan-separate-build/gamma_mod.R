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
  p = 1,
  x = model.matrix(~ tcc, dat_nz),
  y = dat_nz$DRYBIO_AG_TPA_live_ADJ
)


fit_gamma <- stan(file = "stan-models/zi-stan-separate-build/y_mod_gamma.stan",
                  data = stan_list,
                  cores = parallel::detectCores(),
                  iter = 1000,
                  chains = 4)

ext_gamma <- rstan::extract(fit_gamma)


y_mcmc <- data.frame(
  fixed_beta_0 = ext_gamma$beta[ ,1],
  fixed_beta_1 = ext_gamma$beta[ ,2],
  alpha = ext_gamma$alpha
)


new_tcc <- 80

y_mcmc %>% 
  mutate(
    mu = exp(fixed_beta_0 + fixed_beta_1*new_tcc)
  ) %>% 
  mutate(
    # R does this rowwise for us!
    y_hat = rgamma(2000, shape = alpha, rate = mu)
  ) %>% 
  ggplot(aes(x = y_hat)) +
  geom_density()



