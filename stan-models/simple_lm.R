library(rstan)
# run parallel processing
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(here)

dat_raw <- read_rds(here("data", "wa_plots_public.rds"))

dat <- dat_raw %>% 
  select(tcc, tnt, DRYBIO_AG_TPA_live_ADJ, COUNTYCD) %>% 
  mutate(group_id = group_indices(., COUNTYCD))


dat_test <- dat %>% slice_sample(n = 100)
dat_train <- anti_join(dat, dat_test)
  

stan_rf_list <- list(
  n = nrow(dat_train),
  p = 3,
  rfid = dat_train$group_id,
  j = length(unique(dat$group_id)),
  x = model.matrix(~ tcc + tnt, dat_train),
  y = dat_train$DRYBIO_AG_TPA_live_ADJ
)

fit_rf <- stan(file = "stan-models/zi.stan",
               data = stan_rf_list,
               cores = parallel::detectCores(),
               chains = 2)

ext_rf <- rstan::extract(fit_rf)


# stan_list <- list(
#   n = nrow(dat_train),
#   # set to 1 more than number of predictors to include an intercept in the model
#   p = 3,
#   y = dat_train$DRYBIO_AG_TPA_live_ADJ,
#   x = model.matrix(~ tcc + tnt, dat_train)
# )
# fit <- stan(file = "stan-models/lm.stan", 
#             data = stan_list,
#             cores = parallel::detectCores(),
#             chains = 2)
# 
# ext <- rstan::extract(fit)
# 
# ext_df <- cbind(as.data.frame(ext$beta), as.data.frame(ext$sigma))
#
# pred_dists <- list()
# for (i in 1:nrow(dat_test)){
#   pdist <- ext_df %>% 
#     mutate(b1 = dat_test[i, 1], b2 = dat_test[i, 2]) %>% 
#     mutate(mu = V1 + V2*b1 + V3*b2) %>% 
#     rowwise() %>% 
#     mutate(y_new = rnorm(1, mean = mu, sd = `ext$sigma`)) %>% 
#     ungroup() %>% 
#     pull(y_new)
#   
#   pred_dists[[i]] <- pdist
# }



