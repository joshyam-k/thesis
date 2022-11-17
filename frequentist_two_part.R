library(tidyverse)
library(lme4)

data <- sim_data_sets[[2]]

two_part_mod <- function(data) {
  modp <- glmer(Z ~ X + (1 | group), data = data, family = "binomial")
  mody <- glmer(Y ~ X + (1 | group), data = data[data$Z != 0, ], family = Gamma(link = "log"))
  tibble(
    doi = data$group,
    pred = predict(modp, data, type = "response")*predict(mody, data, type = "response")  
  )
}


  
boot_data_gen <- function(force_in) {
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

boot_data <- map(1:10, ~ boot_data_gen(1)) %>% 
  map(two_part_mod)


