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
  mutate(X1 = str_replace(X1, "bags", "")) %>%
  mutate(X1 = str_trim(X1)) %>%
  mutate(X2 = str_replace(X2, "no other bags", "0 NA")) %>%
  mutate(X2 = str_replace(X2, "bags", "")) %>%
  mutate(X2 = str_replace(X2, "bag", "")) %>%
  mutate(X2 = str_trim(X2)) %>%
  separate(X2, c("count", "inside_bag"), sep = "(?<=\\d) ")

find_bags <- function(bag_type) {
  step_results <- bags_allowed %>% filter(inside_bag == bag_type)
  if(count(step_results) > 0 ){
    for(i in 1:nrow(step_results)) {
      parent <- step_results[i,]
      if(count(parent) > 0 && as.numeric(parent$count) != 0) {
        parent_bag <- find_bags(parent$X1)
        if(!is.null(parent_bag)) {
          step_results <- step_results %>%
           add_row(parent_bag)
        }
      }
    }
    return(step_results)
  }
  return()
}

# Part 1
res <- find_bags("shiny gold")
print(length(unique(res$X1)))

# Part 2
bags_allowed <- bags_allowed %>% mutate(count  = as.numeric(count))

count_contained_bags <- function(mult, bag) {
  inside <- filter(bags_allowed, X1 == bag$inside_bag)
  total <- total + (mult * bag$count)
  mult <- mult * bag$count
  if(count(inside) > 0) {
    for(i in 1:nrow(inside)) {
      total <- total + count_contained_bags(mult, inside[i,])
    }
  }
  return(total)
}

shiny_gold <- filter(bags_allowed, X1 == "shiny gold")
total <- 0
for(i in 1:nrow(shiny_gold)) {
  total <- total +  count_contained_bags(1, shiny_gold[i,])
}
print(total)