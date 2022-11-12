library(rstan)
library(tidyverse)
library(here)
options(mc.cores = parallel::detectCores())

# data processing --------------------------------------------------------------

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))



# for now need to remove counties with all zero
dat_raw <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  group_by(COUNTYCD) %>% 
  filter(!all(DRYBIO_AG_TPA_live_ADJ == 0)) %>%
  ungroup() 

counties_subset <- sample(x = dat_raw$COUNTYCD, size = 10)

dat_full <- dat_raw %>% 
  filter(COUNTYCD %in% counties_subset) %>% 
  group_by(COUNTYCD) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup()

stan_list <- list(
  n = nrow(dat_full),
  j = length(unique(dat_full$group_id)),
  p = 2,
  y = dat_full$DRYBIO_AG_TPA_live_ADJ,
  x = model.matrix(~ tcc + tnt, dat_full),
  z = as.integer(as.logical(dat_full$DRYBIO_AG_TPA_live_ADJ)),
  tau_2 = 0.001,
  rfid = dat_full$group_id
)


fit <- stan(file = "stan-models/rstan-built-together/simultaneous_build.stan",
                  data = stan_list,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)



