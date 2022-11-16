library(tidymodels)
library(tidyverse)
library(multilevelmod)
library(bayesrules)
library(rstanarm)

data(cherry_blossom_sample)
running <- cherry_blossom_sample

running_model_1_prior <- stan_glmer(
  net ~ age + (1 | runner), 
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1), 
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735)


as.data.frame(running_model_1_prior)
