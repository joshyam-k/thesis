library(tidyverse)
library(lme4)
library(furrr)
library(tictoc)

data <- sim_data_sets[[2]]

data_train <- data %>%
  slice_head(n = 10000)

data_test <- data %>%
  slice_tail(n = 1000) %>%
  filter(group == 1)


two_part_mod <- function(data, test) {
  modp <- glmer(Z ~ X + (1 | group), data = data, family = "binomial")
  mody <- glmer(Y ~ X + (1 | group), data = data[data$Z != 0, ], family = Gamma(link = "log"))
  return(list(modp, mody))
}

boot_data_gen <- function(data, force_in = 1) {
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

boot_predict <- function(models, test) {
  pred1 <- predict(models[[1]], newdata = test, type = "response")
  pred2 <- predict(models[[2]], newdata = test, type = "response") 
  return(mean(pred1 * pred2))
}



# for n bootstrap iterations
# generate the bootstrap data set and fit the two part model to those data sets
# next use models to predict on new data

n <- 500

plan(multisession, workers = 6)

tic()
boot_data <- future_map(1:n, ~ boot_data_gen(data = data_train), .options = furrr_options(seed = T)) %>% 
  future_map(two_part_mod) %>% 
  future_map_dbl(~ boot_predict(.x, test = data_test))
toc()

tibble(
  dat = boot_data
) %>% 
  ggplot(aes(x = dat)) +
  geom_histogram()
