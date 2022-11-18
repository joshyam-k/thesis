library(tidyverse)
library(lme4)

data <- sim_data_sets[[2]]

data_train <- data %>%
  slice_head(n = 10000)

data_test <- data %>%
  slice_tail(n = 1000) %>%
  filter(group == 1)


two_part_mod <- function(data) {
  modp <- glmer(Z ~ X + (1 | group), data = data, family = "binomial")
  mody <- glmer(Y ~ X + (1 | group), data = data[data$Z != 0, ], family = Gamma(link = "log"))
  list(mody, modp)
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

n <- 100

system.time(
boot_data <- map(1:n, ~ boot_data_gen(data = data_train)) %>% 
  map(two_part_mod) %>% 
  map_dbl(~ boot_predict(.x, test = data_test))
)

tibble(
  dat = boot_data
) %>% 
  ggplot(aes(x = dat)) +
  geom_density()
