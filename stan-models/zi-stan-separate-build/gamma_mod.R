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

ext_gamma <- rstan::extract(fit_gamma)





new_tcc <- 80
new_tnt <- 2
new_grp <- 5

y_mcmc <- data.frame(
  fixed_beta_0 = ext_gamma$betas[ ,1],
  fixed_beta_1 = ext_gamma$betas[ ,2],
  fixed_beta_2 = ext_gamma$betas[ ,3],
  alpha = ext_gamma$alpha,
  u = ext_gamma$u[ ,new_grp]
)


y_mcmc %>% 
  mutate(
    mu = exp(fixed_beta_0 + fixed_beta_1*new_tcc + fixed_beta_2*new_tnt + u)
  ) %>% 
  mutate(
    # R does this rowwise for us!
    y_hat = rgamma(20000, shape = alpha, rate = alpha/mu)
  ) %>% 
  ggplot(aes(x = y_hat)) +
  geom_density()



