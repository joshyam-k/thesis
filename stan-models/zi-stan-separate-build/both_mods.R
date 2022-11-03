library(rstan)
library(tidyverse)
library(here)
# options(mc.cores = parallel::detectCores())

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


# define model inputs (as defined in .stan files) ------------------------------

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

# run models -------------------------------------------------------------------

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

# extracting mcmc results ------------------------------------------------------

ext_y_mod <- rstan::extract(fit_y_mod)
ext_p_mod <- rstan::extract(fit_p_mod)


# posterior distribution of fixed effect coefficient for tcc 

data.frame(
  beta_tcc = ext_y_mod$beta[ ,2]
) %>% 
  ggplot(aes(beta_tcc)) +
  geom_density(fill = "midnightblue", alpha = 0.6) +
  labs(
    x = "Beta (tcc)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12)
  ) 


# getting a predictive posterior distribution ----------------------------------

# setting covariate values
new_tcc <- 80
new_tnt <- 2
group_id <- 8


# dataframe-ing the stan mcmc output
y_mcmc <- data.frame(
  fixed_beta_0 = ext_y_mod$beta[ ,1],
  fixed_beta_1 = ext_y_mod$beta[ ,2],
  fixed_beta_2 = ext_y_mod$beta[ ,3],
  sigma_e = ext_y_mod$sigma_e,
  u = ext_y_mod$u[ , group_id]
)

p_mcmc <- data.frame(
  fixed_gamma_0 = ext_p_mod$gamma[ ,1],
  fixed_gamma_1 = ext_p_mod$gamma[ ,2],
  fixed_gamma_2 = ext_p_mod$gamma[ ,3],
  v = ext_p_mod$v[ , group_id]
)

full_mcmc <- cbind(y_mcmc, p_mcmc)


# generating a prediction across each mcmc iteration to get our posterior
# predictive distribution
posterior_pred_dist <- full_mcmc %>% 
  mutate(
    mu = fixed_beta_0 + fixed_beta_1*new_tcc + fixed_beta_2*new_tnt + u,
    pr = exp(fixed_gamma_0 + fixed_gamma_1*new_tcc + fixed_gamma_2*new_tnt + v)/
      (1 + exp(fixed_gamma_0 + fixed_gamma_1*new_tcc + fixed_gamma_2*new_tnt + v))
  ) %>% 
  mutate(
    # R does this rowwise for us!
    y_hat = rnorm(20000, mean = mu, sd = sigma_e),
    z_hat = rbinom(20000, 1, pr),
    y_final = y_hat*z_hat
  ) %>% 
  select(y_final)

# plot the distribution
posterior_pred_dist %>% 
  ggplot(aes(x = y_final)) +
  geom_density(fill = "midnightblue", alpha = 0.6) +
  labs(
    x = "Y_hat_final",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12)
  ) 







  




