library(tidyverse)
library(rstan)
library(lme4)
library(furrr)
library(tictoc)
options(mc.cores = parallel::detectCores())

# replace with number of active cores your computer can use
plan(multisession, workers = 6)
# set a seed
set.seed(10110)


DGP_s1 <- function(n, g) {
  
  m <- n/g
  X <- rnorm(n, 0, 1)
  group <- rep(seq(1, g), m) 
  
  # Generate REs
  gamma_y <- rnorm(g, 0, 1) # for the y-model
  gamma_z <- rnorm(g, 0, 1) # for the z-model
  
  # Make Z -- 1 means Y>0, 0 means Y=0
  beta_z <- 1
  xb <- 1.5 + X*beta_z + gamma_z[group] # Adding 1 so we have <50% 0's
  prob <- exp(xb) / (1 + exp(xb)) # Probability of being non-zero
  Z <- 1*(runif(n)<prob) # simulating the Z
  
  # Make Y
  beta_y <- 1
  ypure <- 10 + X*beta_y + gamma_y[group] 
  
  Y <- rgamma(n, shape = 3, rate = 1/ypure) # Before accounting for Z
  Y <- Z*Y # If Z=0, then Y=0
  
  return(tibble(X, Y, Z, group))
  
}


data <- DGP_s1(n = 3000, g = 20)

grp <- 5

data_train <- data %>% 
  slice_head(n = 2000)

data_test <- data %>% 
  slice_tail(n = 1000) %>%
  # choosing a group to test on
  filter(group == grp) 

data_nz <- data_train %>% 
  filter(Y > 0)

stan_list_mod1 <- list(
  n = nrow(data_nz),
  p = 1,
  rfid = data_nz$group,
  j = length(unique(data_nz$group)),
  x = model.matrix(~ X, data_nz),
  y = data_nz$Y
)

stan_list_mod2 <- list(
  n = nrow(data_train),
  p = 1,
  rfid = data_train$group,
  j = length(unique(data_train$group)),
  x = model.matrix(~ X, data_train),
  z = data_train$Z
)

fit_gamma <- stan(file = "stan-models/zi-stan-separate-build/y_mod_gamma.stan",
                  data = stan_list_mod1,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)

fit_p_mod <- stan(file = "stan-models/zi-stan-separate-build/p_mod2.stan",
                  data = stan_list_mod2,
                  cores = parallel::detectCores(),
                  iter = 10000,
                  chains = 4)

ext_y_mod <- rstan::extract(fit_gamma)
ext_p_mod <- rstan::extract(fit_p_mod)

y_mcmc <- data.frame(
  fixed_beta_0 = ext_y_mod$betas[ ,1],
  fixed_beta_1 = ext_y_mod$betas[ ,2],
  alpha = ext_y_mod$alpha,
  u = ext_y_mod$u[ ,grp]
)

p_mcmc <- data.frame(
  fixed_gamma_0 = ext_p_mod$gamma[ ,1],
  fixed_gamma_1 = ext_p_mod$gamma[ ,2],
  v = ext_p_mod$v[ ,grp]
)

## for a large number of iterations, shuffle y_mcmc before doing a cbind 
## predict on a single point and store that predictive distribution

post_pred_res <- list()
for(i in 1:100){
  
  shuffled_y <- y_mcmc[sample(1:nrow(y_mcmc)), ]
  shuffled_p <- p_mcmc[sample(1:nrow(p_mcmc)), ]
  rownames(shuffled_y) <- NULL
  rownames(shuffled_p) <- NULL
  full_mcmc <- cbind(shuffled_y, shuffled_p)
  
  posterior_pred_dist <- full_mcmc |>
    mutate(
      mu = exp(fixed_beta_0 + fixed_beta_1*data_test$X[1]+ u),
      
      pr = exp(fixed_gamma_0 + fixed_gamma_1*data_test$X[1] + v)/
        (1 + exp(fixed_gamma_0 + fixed_gamma_1*data_test$X[1] + v))
    ) |>
    mutate(
      # R does this rowwise for us!
      y_hat = rgamma(20000, shape = alpha, rate = alpha/mu),
      z_hat = rbinom(20000, 1, pr),
      y_final = y_hat*z_hat
    ) %>% 
    mutate(run = i) %>% 
    select(run, y_final) 
  
  post_pred_res[[i]] <- posterior_pred_dist
  
}

post_pred_df <- post_pred_res |>
  map_dfr(rbind)

post_pred_df |>
  ggplot(aes(x = y_final, group = run)) +
  geom_density()





