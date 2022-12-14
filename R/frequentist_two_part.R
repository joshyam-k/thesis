library(tidyverse)
library(lme4)
library(furrr)
data <- sim_data_sets[[1]]

data_train <- data %>%
  slice_head(n = 2000)

data_test <- data %>%
  slice_tail(n = 1000) %>% 
  filter(group == 5)


boot_data_gen <- function(data, force_in = 5) {
  grp1 <- sample(
    x = unique(data_train$group)[-force_in],
    size = 1
    )
  grps <- sample(
    x = unique(data$group),
    size = length(unique(data$group)) - 2,
    replace = T
  )
  tibble(
    # force a group to be in the sample
    group = c(force_in, grp1, grps)
  ) %>% 
    left_join(data, by = "group")
}


two_part_mod <- function(data) {
  modp <- glmer(Z ~ X + (1 | group), data = data, family = "binomial")
  mody <- glmer(Y ~ X + (1 | group), data = data[data$Z != 0, ], family = Gamma(link = "log"))
  return(list(modp, mody))
}

boot_predict <- function(models, test) {
  coef1 <- coef(models[[1]])$group[5, ]
  coef2 <- coef(models[[2]])$group[5, ]
  
  test %>%
    mutate(p_hat = exp(coef1$X*X + coef1$`(Intercept)`)/(1 + exp(coef1$X*X + coef1$`(Intercept)`)),
           y_hat = coef2$X*X + coef2$`(Intercept)`,
           full_hat = y_hat*p_hat) %>% 
    summarise(y_final = mean(full_hat)) %>% 
    pull()
  
  #pred1 <- predict(models[[1]], newdata = test, type = "response")
  #pred2 <- predict(models[[2]], newdata = test, type = "response") 
  #return(mean(pred1 * pred2))
}

boot <- boot_data_gen(data_train)

mods <- two_part_mod(data_train)

boot_predict(mods, data_test)


# for n bootstrap iterations
# generate the bootstrap data set and fit the two part model to those data sets
# next use models to predict on new data

n <- 400
plan(multisession, workers = 6)

boot_data <- future_map(1:n, ~ boot_data_gen(data = data_train), .options = furrr_options(seed = T)) %>% 
  future_map(two_part_mod) %>% 
  future_map_dbl(~ boot_predict(.x, test = data_test)) 

quantile(boot_data, probs = c(0.025, 0.975))


tibble(
  x = boot_data
) %>% 
  ggplot(aes(x = x)) +
  geom_density() +
  geom_vline(xintercept = mean(data_test$Y), color = "red", size = 2, alpha = 0.4) 

ggplot() +
  geom_density(data = sim_res[[1]]$result, aes(x = y_hat), color = "black", linewidth = 2) +
  geom_density(data = boot_d, aes(x = x), color = "blue") +
  geom_vline(xintercept = mean(data_test$Y), color = "red", size = 2, alpha = 0.4) 
