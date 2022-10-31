library(rstanarm)
library(tidyverse)
library(here)

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))

dat <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  group_by(COUNTYCD) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(DRYBIO_AG_INDICATOR = ifelse(DRYBIO_AG_TPA_live_ADJ > 0, 1, 0))

dat_y_mod <- dat %>% 
  filter(DRYBIO_AG_TPA_live_ADJ > 0)

mod_y <- stan_glmer(
  DRYBIO_AG_TPA_live_ADJ ~ tcc + (1 | group_id), 
  data = dat_y_mod, family = gaussian,
  prior_intercept = normal(100, 100),
  prior = normal(2.5, 100), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  cores = parallel::detectCores()
)

mod_p <- stan_glmer(
  DRYBIO_AG_INDICATOR ~ tcc + (1 | group_id), 
  data = dat, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  cores = parallel::detectCores()
)


res <- as.data.frame(mod_p)

res %>% 
  ggplot(aes(x = `b[(Intercept) group_id:2]`)) +
  geom_density()
