# loading packages
# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

# simulate non grouped data
sim_dat <- tibble(
  x = rnorm(1000, 50, 9),
  error = rnorm(1000, 0, 36),
  y = 150 + 4*x + error
)

sim_dat %>% 
  ggplot(aes(x, y)) +
  geom_point()

model <- stan_glm(y ~ x, data = sim_dat,
                  family = gaussian,
                  prior_intercept = normal(350, 100),
                  prior = normal(10, 40), 
                  prior_aux = exponential(0.008),
                  chains = 4, iter = 5000*2, seed = 84735)


tidy(model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.90)

model_df <- as.data.frame(model)

# Predict rides for each parameter set in the chain
# on a new set of data

set.seed(10)
x_new <- runif(50, 30, 75)

res_list <- list()
means <- c()

for(i in 1:length(x_new)){
  curr <- model_df %>% 
    mutate(newd = x_new[i]) %>% 
    mutate(mu = `(Intercept)` + x*newd) %>% 
    rowwise() %>% 
    mutate(y_new = rnorm(1, mean = mu, sd = sigma)) %>% 
    ungroup()
  
  res_list[[i]] <- curr
  m_i <- mean(curr$y_new)
  means <- c(means, m_i)
  
}

# from here we can get single point predictions

res_list[[1]] %>% 
  summarise(y_pred = mean(y_new))


# predictive distribution of an average?
# average over the group (does the average over the runs then give you a point estimate prediction of the group level mean)
data.frame(
  m = means
) %>% 
  ggplot(aes(x = m)) +
  geom_density()

# compare to this

res_list[[1]] %>% 
  ggplot(aes(x = y_new)) +
  geom_density()



## grouped data

data(cherry_blossom_sample)
running <- cherry_blossom_sample %>% 
  select(runner, age, net)

ggplot(running, aes(y = net, x = age)) + 
  geom_point()


wrong_model <- stan_glm(
  net ~ age, 
  data = running, family = gaussian, 
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

# Posterior summary statistics
tidy(wrong_model, conf.int = TRUE, conf.level = 0.80)


# Plot of the posterior median model
ggplot(running, aes(x = age, y = net, group = runner)) + 
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  geom_abline(aes(intercept = 75.2, slope = 0.268), color = "blue")


wrong_model_df <- as.data.frame(wrong_model)


# predictions for new runners
set.seed(100)
new_dat <- runif(50, 50, 60)


new_res_list <- list()
new_means <- c()

for(i in 1:length(new_dat)){
  curr <- wrong_model_df %>% 
    mutate(newd = new_dat[i]) %>% 
    mutate(mu = `(Intercept)` + age*newd) %>% 
    rowwise() %>% 
    mutate(y_new = rnorm(1, mean = mu, sd = sigma)) %>% 
    ungroup()
  
  new_res_list[[i]] <- curr
  m_i <- mean(curr$y_new)
  new_means <- c(new_means, m_i)
  
}



