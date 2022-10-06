library(tidyverse)
library(rstan)
library(rstanarm)

## Setting 1
DGP <- function(n){
  X <- rnorm(n, 50, 9)
  beta <- 4
  eps <- sqrt(2)*sd(X*beta)*rnorm(n, 0, 1)
  Y <- X*beta + eps
  
  data <- tibble(X, Y) 
  return(data)
}

covers <- 0
set.seed(10110)

for (t in 1:1000){
  s1_original <- DGP(1000)
  
  model <- stan_glm(Y ~ X, data = s1_original,
                    family = gaussian,
                    prior_intercept = normal(200, 60),
                    prior = normal(5, 20), 
                    prior_aux = exponential(0.02),
                    chains = 4, iter = 5000*2, seed = 84735)
  
  s1_new <- DGP(50)
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
  
  true_mean <- mean(s1_new$Y)
  if(true_mean > q[1] & true_mean < q[2]) {
    covers = covers + 1
  }
  
  print(paste0("done with trial ", t))
  
}


## Setting 2










