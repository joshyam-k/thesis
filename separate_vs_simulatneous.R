library(rstan)
library(tidyverse)
library(here)
library(janitor)
options(mc.cores = parallel::detectCores())

# data processing --------------------------------------------------------------

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))

# for now need to remove counties with all zero
dat_raw <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  group_by(COUNTYCD) %>% 
  filter(!all(DRYBIO_AG_TPA_live_ADJ == 0)) %>%
  ungroup() 

# downsize
counties_subset <- sample(x = unique(dat_raw$COUNTYCD), size = 10)

dat_full <- dat_raw %>% 
  filter(COUNTYCD %in% counties_subset) %>% 
  group_by(COUNTYCD) %>% 
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


fit_normal <- stan(file = "stan-models/zi-stan-separate-build/y_mod1.stan",
                  data = stan_list_mod1,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)


fit_p_mod <- stan(file = "stan-models/zi-stan-separate-build/p_mod2.stan",
                  data = stan_list_mod2,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)


stan_simultaneous <- list(
  n = nrow(dat_full),
  j = length(unique(dat_full$group_id)),
  p = 2,
  y = dat_full$DRYBIO_AG_TPA_live_ADJ,
  x = model.matrix(~ tcc + tnt, dat_full),
  z = as.integer(as.logical(dat_full$DRYBIO_AG_TPA_live_ADJ)),
  tau_2 = 0.0001,
  rfid = dat_full$group_id
)


fit_simultaneous <- stan(file = "stan-models/rstan-built-together/simultaneous_build_normal.stan",
            data = stan_simultaneous,
            cores = parallel::detectCores(),
            iter = 10000,
            chains = 4)



simul <- as.data.frame(fit_simultaneous) %>% 
  clean_names() %>% 
  select(-c(tau_1, lp)) %>% 
  mutate(model = "simultaneous")

y <- as.data.frame(fit_normal) %>% 
  clean_names() %>% 
  select(-c(sigma_e, lp))

p <- as.data.frame(fit_p_mod) %>% 
  clean_names() %>% 
  select(-lp) %>% 
  mutate(model = "separate")

sep <- cbind(y, p)

full_comp <- rbind(simul, sep) %>% 
  pivot_longer(cols = beta_1:sigma_v, names_to = "param", values_to = "value")

full_comp %>% 
  ggplot(aes(x = value, fill = model)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~param, scales = "free") +
  labs(
    title = "Simultaneous vs Separate Bayesian model builds",
    subtitle = "u is associated with the linear model, v the logistic regression \n sigma_u and sigma_v are the sd of the random effects of each respective model"
  )
