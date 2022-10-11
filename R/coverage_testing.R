library(tidyverse)
library(rstan)
library(rstanarm)

# seed for all sims
set.seed(10110)

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
for (t in 1:50){
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
  
  X <- rnorm(n, 50, 9)
  beta <- rnorm(1, 10, 1)
  sigma <- rnorm(1, 160, 16)
  eps <- rnorm(n, 0, sigma)
    
  Y <- X*beta + eps
  
  return(tibble(X, Y))
  
}





covers_s2 <- data.frame()
for (t in 1:500){
  s2_full <- DGP_s2(1050)
  s2_build <- s2_full %>% head(1000)
  
  model <- stan_glm(Y ~ X, data = s2_build,
                    family = gaussian,
                    prior_intercept = normal(200, 60),
                    prior = normal(10, 20), 
                    prior_aux = exponential(0.002),
                    chains = 4, iter = 5000*2, seed = 84735)
  
  s2_test <- s2_full %>% tail(50)
  model_df <- as.data.frame(model)
  
  full_preds <- data.frame()
  for(i in 1:nrow(s2_test)){
    curr <- model_df %>% 
      mutate(newd = s2_test$X[i]) %>% 
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
    true = mean(s2_test$Y),
    lower = q[1],
    upper = q[2]
  )
  
  covers_s2 <- rbind(covers_s2, to_add) 
  
  print(paste0("done with trial ", t))
  
}







## Setting 3
# input n should be a number divisible by 10
DGP_s3 <- function(n){
  
  X <- rnorm(n, 50, 9)
  beta <- 4
  # number of groups
  g <- 50  
  # number of observations per group
  m <- n/g
  # group number for data
  group <- rep(seq(1, g), m) 
  gamma <- rnorm(g, 0, 36) 
  
  ypure <- X*beta + gamma[group] 
  Y <- ypure + sqrt(2)*sd(ypure)*rnorm(n, 0, 1)
  
  return(tibble(X, Y, group))
  
}




covers_s3 <- data.frame()

for (t in 1:500){
  s3_full <- DGP_s3(10500)
  s3_build <- s3_full %>% head(10000)
  
  model <- stan_glm(Y ~ X, data = s3_build,
                    family = gaussian,
                    prior_intercept = normal(200, 60),
                    prior = normal(4, 16), 
                    prior_aux = exponential(0.02),
                    chains = 4, iter = 5000*2, seed = 84735)
  
  # set to 500 so we get 50 obs in group 1
  s3_test <- s3_full %>% tail(500) %>% 
    filter(group == 1)
    
  model_df <- as.data.frame(model)
  
  full_preds <- data.frame()
  for(i in 1:nrow(s3_test)){
    curr <- model_df %>% 
      mutate(newd = s3_test$X[i]) %>% 
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
    true = mean(s3_test$Y),
    lower = q[1],
    upper = q[2]
  )
  
  covers_s3 <- rbind(covers_s3, to_add) 
  
  print(paste0("done with trial ", t))
  
}



## Setting 4
# still using DGP from setting 3

covers_s4 <- data.frame()

for(t in 1:50){
  
  s4_full <- DGP_s3(12500)
  s4_build <- s4_full %>% head(10000)
  s4_test <- s4_full %>% tail(2500) %>% 
    filter(group == 1)
  
  model <- stan_glmer(
    Y ~ X + (1 | group), 
    data = s4_build, family = gaussian,
    prior_intercept = normal(200, 10),
    prior = normal(4, 2), 
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735, 
    prior_PD = F)
  
  model_df <- as.data.frame(model)
  full_preds <- data.frame()
  
  for(i in 1:nrow(s4_test)) {
    curr <- model_df %>% 
      rename(b = `b[(Intercept) group:1]`) %>% 
      select(`(Intercept)`, X, b, sigma) %>% 
      mutate(mu_grp1 = `(Intercept)` + X*s4_test$X[i] + b,
             y_new = rnorm(20000, mu_grp1, sd = sigma)) %>% 
      mutate(iter = row_number())
    
    full_preds <- rbind(full_preds, curr)
    
  }
  
  full_preds_means <- full_preds %>% 
    group_by(iter) %>% 
    summarise(pred = mean(y_new)) 
  
  q <- quantile(full_preds_means$pred, probs = c(0.025, 0.975))
  
  to_add <- tibble(
    true = mean(s4_test$Y),
    lower = q[1],
    upper = q[2]
  )
  
  covers_s4 <- rbind(covers_s4, to_add) 
  
  print(paste0("done with trial ", t))
  
 

}




# helper function to extract coverage stat
coverage <- function(df){
  df %>% 
    rowwise() %>% 
    mutate(falls_in = between(true, lower, upper)) %>% 
    ungroup() %>% 
    summarise(coverage = mean(falls_in))
}





