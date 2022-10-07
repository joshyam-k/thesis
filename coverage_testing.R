library(tidyverse)
library(rstan)
library(rstanarm)

## Setting 1
DGP_s1 <- function(n){
  X <- rnorm(n, 50, 9)
  beta <- 4
  eps <- sqrt(2)*sd(X*beta)*rnorm(n, 0, 1)
  Y <- X*beta + eps
  
  data <- tibble(X, Y) 
  return(data)
}

covers_s1 <- data.frame()
set.seed(10110)

for (t in 1:250){
  s1_original <- DGP_s1(1000)
  
  model <- stan_glm(Y ~ X, data = s1_original,
                    family = gaussian,
                    prior_intercept = normal(200, 60),
                    prior = normal(5, 20), 
                    prior_aux = exponential(0.02),
                    chains = 4, iter = 5000*2, seed = 84735)
  
  s1_new <- DGP_s1(50)
  model_df <- as.data.frame(model)
  
  full_preds <- data.frame()
  for(i in 1:nrow(s1_new)){
    curr <- model_df %>% 
      mutate(newd = s1_new$X[i]) %>% 
      mutate(mu = `(Intercept)` + X*newd) %>% 
      rowwise() %>% 
      mutate(y_new = rnorm(1, mean = mu, sd = sigma)) %>% 
      ungroup() %>% 
      mutate(iter = row_number())
    
    full_preds <- rbind(full_preds, curr)
    
  }
  
  full_preds_means <- full_preds %>% 
    group_by(iter) %>% 
    summarise(pred = mean(y_new)) 
  
  q <- quantile(full_preds_means$pred, probs = c(0.025, 0.975))
  
  to_add <- tibble(
    true = mean(s1_new$Y),
    lower = q[1],
    upper = q[2]
  )
  
  covers_s1 <- rbind(covers_s1, to_add) 
  
  print(paste0("done with trial ", t))
  
}


## Setting 2


DGP_s2 <- function(n){
  
  X <- rnorm(1000, 50, 9)
  beta <- rnorm(1000, 10, 4)
  sigma <- rnorm(1000, 200, 30)
  
  # there's probably a simpler way to do this but its sufficiently fast 
  eps <- tibble(
    sigma
  ) %>% 
    rowwise() %>% 
    mutate(eps = rnorm(1, 0, sigma)) %>% 
    ungroup() %>% 
    pull(eps)
  
  Y <- X*beta + eps
  
  return(tibble(X, Y))
  
}


covers_s2 <- data.frame()
set.seed(10111)

for (t in 1:250){
  s2_original <- DGP_s2(1000)
  
  model <- stan_glm(Y ~ X, data = s2_original,
                    family = gaussian,
                    prior_intercept = normal(500, 60),
                    prior = normal(10, 20), 
                    prior_aux = exponential(0.002),
                    chains = 4, iter = 5000*2, seed = 84735)
  
  s2_new <- DGP_s2(50)
  model_df <- as.data.frame(model)
  
  full_preds <- data.frame()
  for(i in 1:nrow(s2_new)){
    curr <- model_df %>% 
      mutate(newd = s2_new$X[i]) %>% 
      mutate(mu = `(Intercept)` + X*newd) %>% 
      rowwise() %>% 
      mutate(y_new = rnorm(1, mean = mu, sd = sigma)) %>% 
      ungroup() %>% 
      mutate(iter = row_number())
    
    full_preds <- rbind(full_preds, curr)
    
  }
  
  full_preds_means <- full_preds %>% 
    group_by(iter) %>% 
    summarise(pred = mean(y_new)) 
  
  q <- quantile(full_preds_means$pred, probs = c(0.025, 0.975))
  
  to_add <- tibble(
    true = mean(s2_new$Y),
    lower = q[1],
    upper = q[2]
  )
  
  covers_s2 <- rbind(covers_s2, to_add) 
  
  print(paste0("done with trial ", t))
  
}












