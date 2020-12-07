library("tidyr")
library("readr")
library("dplyr")
library("stringr")
library("purrr")
data <- read_file("input.txt") %>%
  str_replace_all(" contain ", "\t") %>%
  read_tsv(col_names = FALSE) %>%
  mutate(X2 = str_remove_all(X2, "\\.")) %>%
  mutate(X2 = str_split(X2, ","))

bags_allowed <- data %>%
  unnest(X2) %>%
  mutate(X2 = str_replace(X2, "no other bags", "0 no other bags")) %>%
  mutate(X2 = str_trim(X2)) %>%
  separate(X2, c("count", "inside_bag"), sep = "(?<=\\d) ")

find_bags <- function(bag_type) {
  step_results <- bags_allowed %>% filter(inside_bag == bag_type)
  for(i in 1:nrow(step_results)) {
    
  }
  
}
  
  
  
 