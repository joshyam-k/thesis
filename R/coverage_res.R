library(tidyverse)
library(here)


s1_res <- read_csv(here("data", "covers_s1.csv"))
s2_res <- read_csv(here("data", "covers_s2.csv"))
s3_res <- read_csv(here("data", "covers_s3.csv"))
s4_res <- read_csv(here("data", "covers_s4.csv"))



# helper function to extract coverage stat
coverage <- function(df){
  df %>% 
    rowwise() %>% 
    mutate(falls_in = between(true, lower, upper)) %>% 
    ungroup() %>% 
    summarise(coverage = mean(falls_in))
}


# expect about right
coverage(s1_res)

# expect under coverage
coverage(s2_res)


coverage(s3_res)

coverage(s4_res)





