install.packages("tidyverse")
library(tidyr)
library(dplyr)

read.csv("input.csv", col.names = c("x")) %>% 
  mutate(y = x) %>% 
  expand(x,y) %>% 
  mutate(sum = rowSums(.[1:2])) %>% 
  filter(sum == 2020) %>% 
  mutate(product = x*y) %>%
  print()

read.csv("input.csv", col.names = c("x")) %>% 
  mutate(y = x) %>% 
  mutate(z = x) %>% 
  expand(x,y,z) %>% 
  mutate(sum = rowSums(.[1:3])) %>% 
  filter(sum == 2020) %>% 
  mutate(product = x*y*z) %>%
  print()
