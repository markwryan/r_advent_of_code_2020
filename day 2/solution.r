install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(stringr)
  
# Read and clean up the data
data <- read.table("input.txt", col.names = c("range", "requirement", "password")) %>%
  mutate(requirement = substr(requirement,1,1)) %>%
  separate(range, c("min", "max"), "-") %>%
  mutate(min = as.numeric(min)) %>%
  mutate(max = as.numeric(max))

# Part 1
data$occurances <- str_count(data$password, data$requirement)
valid <- filter(data, occurances >= min & occurances <= max)
count(valid)

# Part 2
valid_part2 <- data %>%
  mutate(position1 = str_sub(password, min, min)) %>%
  mutate(position2 = str_sub(password, max, max)) %>%
  filter((position1 == requirement & position2 != requirement) | (position1 != requirement & position2 == requirement))
count(valid_part2)
