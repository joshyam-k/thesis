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


# setting up sim data sets
sim_data_sets <- list()
for (i in 1:1) {
  # train on 2000 test on 1000 (but only on one group, so 50)
  sim_data_sets[[i]] <- DGP_s1(n = 3000, g = 20)
}



model_build <- function(data) {
  
  grp <- 5
  
  data_train <- data %>% 
    slice_head(n = 2000)
  
  data_test <- data %>% 
    slice_tail(n = 1000) %>%
    # choosing a group to test on
    filter(group == grp) 
  
  data_nz <- data_train %>% 
    filter(Y > 0)
  
  ## bayesian ------------------------------------------------------------------
  
  start_bayes <- Sys.time()
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

  full_preds <- data.frame(
    y_final = rep(0, nrow(data_test)*20000),
    iter = rep(0, nrow(data_test*20000))
    )

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

  q_bayes <- quantile(full_group_preds$y_hat, probs = c(0.025, 0.975))

  end_bayes <- Sys.time()
  time_bayes <- as.numeric(end_bayes - start_bayes)

  out_bayes <- tibble(
    y_hat_mean = mean(full_group_preds$y_hat),
    lower = q_bayes[1],
    upper = q_bayes[2],
    y_true = full_group_preds$y_true[1],
    duration = time_bayes,
    model = "b"
  )




  ## Frequentist ---------------------------------------------------------------

  start_freq <- Sys.time()
  # setting up helper functions
  boot_data_gen <- function(data, force_in = 5) {
    grps <- sample(
      x = unique(data$group),
      size = length(unique(data$group)) - 1,
      replace = T
    )
    tibble(
      # force a group to be in the sample
      group = c(force_in, grps)
    ) %>%
      left_join(data, by = "group")
  }

  two_part_mod <- function(data) {
    modp <- glmer(Z ~ X + (1 | group), data = data, family = "binomial")
    mody <- glmer(Y ~ X + (1 | group), data = data[data$Z != 0, ], family = Gamma(link = "log"))
    return(list(modp, mody))
  }

  boot_predict <- function(models, data) {
    pred1 <- predict(models[[1]], newdata = data, type = "response")
    pred2 <- predict(models[[2]], newdata = data, type = "response")
    return(mean(pred1 * pred2))
  }

  # get original prediction
  original_fit <- two_part_mod(data_train)
  pred1 <- predict(original_fit[[1]], newdata = data_test, type = "response")
  pred2 <- predict(original_fit[[2]], newdata = data_test, type = "response")
  original_pred <- mean(pred1 * pred2)

  # run boostrapping
  # returns a vector of length 400 with the mean of the predictions
  boot_data <- future_map(
    1:300,
    ~ boot_data_gen(data = data_train),
    .options = furrr_options(seed = T)
  ) %>%
    future_map(
      two_part_mod
    ) %>%
    future_map_dbl(
      ~ boot_predict(.x, data = data_test)
    )

  q_freq <- quantile(boot_data, probs = c(0.025, 0.975))

  end_freq <- Sys.time()
  time_freq <- as.numeric(end_freq - start_freq)

  out_freq <- tibble(
    y_hat_mean = original_pred,
    lower = q_freq[1],
    upper = q_freq[2],
    y_true = mean(data_test$Y),
    duration = time_freq,
    model = "f"
  )

  out_full <- rbind(out_bayes, out_freq)
  
  return(out_full)

}


sim_res <- sim_data_sets %>% 
  future_map(.f = safely(model_build), .options = furrr_options(seed = T))


full_res <- data.frame()
for(i in 1:50){
  full_res <- rbind(full_res, sim_res[[i]]$result)
}

full_res %>% 
  group_by(model) %>% 
  rowwise() %>% 
  mutate(falls_in = between(y_true, lower, upper)) %>% 
  ungroup() %>% 
  group_by(model) %>% 
  summarise(coverage = mean(falls_in))






















