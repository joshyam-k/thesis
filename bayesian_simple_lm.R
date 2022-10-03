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
     conf.int = TRUE, conf.level = 0.80)

model_df <- as.data.frame(model)

# Predict rides for each parameter set in the chain
# when x is 70
set.seed(84735)
predict_70 <- model_df %>% 
  mutate(mu = `(Intercept)` + x*70,
         y_new = rnorm(20000, mean = mu, sd = sigma))


ggplot(predict_70, aes(x = y_new)) + 
  geom_density()

## now for a set of new data

x_new = runif(100, 30, 75)


