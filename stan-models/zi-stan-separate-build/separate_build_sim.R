library(tidyverse)
library(rstan)
library(lme4)

set.seed(10110)

## Setting 1 -- Bayesian Model should correct specify
# n is number of total observations, g is number of groups. n should be divisible by g.
DGP_s1 <- function(n, g){
  
  # number of observations per group
  m <- n/g
  
  # Create x
  X <- rnorm(n, 0, 1)
  
  # Assign Groups
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
  
  # Return dataset
  return(tibble(X, Y, Z, group))
  
}


sim_data_sets <- list()
for (i in 1:100) {
  # using n = 11000 because we will hold out the last 1000 to predict on
  sim_data_sets[[i]] <- DGP_s1(n = 11000, g = 10)
}



test <- sim_data_sets[[2]]

modp <- glmer(Z ~ X + (1 | group), data = test, family = "binomial")
mody <- glmer(Y ~ X + (1 | group), data = test[test$Z != 0, ], family = Gamma(link = "log"))

tibble(
  doi = test$group,
  pred = predict(modp, test, type = "response")*predict(mody, test, type = "response")  
) %>% 
  group_by(doi) %>% 
  summarise(pred_grp = mean(pred)) %>% 
  ungroup()





model_build <- function(data) {
  
  grp <- 5
  
  data_train <- data %>% 
    slice_head(n = 10000)
  
  data_test <- data %>% 
    slice_tail(n = 1000) %>%
    # choosing a group to test on
    filter(group == grp) 
  
  data_nz <- data_train %>% 
    filter(Y > 0)
  
  ## zi
  
  
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
  
  full_mcmc <- cbind(y_mcmc, p_mcmc)
  
  full_preds <- data.frame(y_final = rep(0, nrow(data_test)*20000), iter = rep(0, nrow(data_test*20000)))
  j <- 1
  
  for (i in 1:nrow(data_test)) {
    posterior_pred_dist <- full_mcmc %>% 
      mutate(
        mu = exp(fixed_beta_0 + fixed_beta_1*data_test$X[i]+ u),
        
        pr = exp(fixed_gamma_0 + fixed_gamma_1*data_test$X[i] + v)/
          (1 + exp(fixed_gamma_0 + fixed_gamma_1*data_test$X[i] + v))
      ) %>% 
      mutate(
        # R does this rowwise for us!
        y_hat = rgamma(20000, shape = alpha, rate = alpha/mu),
        z_hat = rbinom(20000, 1, pr),
        y_final = y_hat*z_hat
      ) %>% 
      mutate(iter = row_number()) %>% 
      select(y_final, iter)
    
    full_preds[j:(j + 20000 - 1), ] <- posterior_pred_dist
    
    j <- j + 20000
  }
  
  full_group_preds <- full_preds %>% 
    group_by(iter) %>% 
    summarise(y_hat = mean(y_final)) %>% 
    mutate(y_true = mean(data_test$Y))
    
  
  return(full_group_preds)
  
}

sim_res <- sim_data_sets %>% 
  map(.f = model_build)


summary_results <- data.frame(run = rep(0, 100), y_hat_mean = rep(0, 100), y_hat_median = rep(0, 100),
                              lower = rep(0, 100), upper = rep(0, 100), y_true = rep(0, 100))

for (i in 1:100) {
  q <- quantile(sim_res[[i]]$y_hat, probs = c(0.025, 0.975))
  out <- tibble(
    run = i,
    y_hat_mean = mean(sim_res[[i]]$y_hat),
    y_hat_median = median(sim_res[[i]]$y_hat),
    lower = q[1],
    upper = q[2],
    y_true = sim_res[[i]]$y_true[1]
  )
  
  summary_results[i, ] <- out
  
}







## Results

sim_res <- read_csv("~/Desktop/thesis/bayes_two_part_sim_res.csv")


### Coverage 

sim_res %>% 
  rowwise() %>% 
  mutate(falls_in = between(y_true, lower, upper)) %>% 
  ungroup() %>% 
  summarise(coverage = mean(falls_in))

### Bias

sim_res %>% 
  mutate(indv = y_hat_mean - y_true) %>% 
  ggplot(aes(x = indv))+
  geom_density()















