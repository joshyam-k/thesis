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


dat %>% 
  ggplot(aes(x = tcc, y = DRYBIO_AG_TPA_live_ADJ)) +
  geom_point()

dat_y_mod <- dat %>% 
  filter(DRYBIO_AG_TPA_live_ADJ > 0)

mod_y <- stan_glmer(
  DRYBIO_AG_TPA_live_ADJ ~ tcc + tnt + (1 | group_id), 
  data = dat_y_mod, family = gaussian,
  prior_intercept = normal(100, 100),
  prior = normal(2.5, 100), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  cores = parallel::detectCores()
)

mod_p <- stan_glmer(
  DRYBIO_AG_INDICATOR ~ tcc + tnt + (1 | group_id), 
  data = dat, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  cores = parallel::detectCores()
)


res_p <- as.data.frame(mod_p)
res_y <- as.data.frame(mod_y)


new_x <- 30

prediction <- function(new_x, group, df_p, df_y) {
  
    name_match <- paste0("b[(Intercept) group_id:", group, "]")
    
    # select correct group col
    df_y <- select(df_y, c(name_match, `(Intercept)`, tcc, sigma)) %>% 
      rename(b = name_match) 
    
    df_p <- select(df_p, c(name_match, `(Intercept)`, tcc)) %>% 
      rename(b = name_match)
    
    names(df_y) <- paste0(names(df_y), "_y")
    names(df_p) <- paste0(names(df_p), "_p")
    
    full_params <- cbind(df_y, df_p)

    full_params %>% 
      mutate(
        mu = b_y + `(Intercept)_y` + tcc_y*new_x,
        pr = (exp(b_p + `(Intercept)_p` + tcc_p*new_x)/(1 + exp(b_p + `(Intercept)_p` + tcc_p*new_x))),
        y_hat = rnorm(20000, mean = mu, sd = sigma_y),
        p_hat = rbinom(20000, 1, pr),
        y_final = y_hat*p_hat
      ) 
}

prediction(10, 20, res_p, res_y) %>% 
  ggplot(aes(x = y_final)) +
  geom_density()


    


