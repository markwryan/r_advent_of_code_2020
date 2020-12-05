library(tidyr)
library(dplyr)
library(stringr)

as_decimal <- function(binary) {
  binary <- rev(binary)
  total <- 0
  for(j in 1:length(binary)) {
    c <- as.numeric(binary[j])
    total <-total + (c*(2^(j-1)))
  }
  return(total)
}

data <- read.csv("input.csv", header=FALSE) %>%
  mutate(row = str_sub(V1,1,7)) %>%
  mutate(column = str_sub(V1,8,11)) %>%
  mutate(row = str_replace_all(row,"F","0")) %>%
  mutate(row = str_replace_all(row,"B", "1")) %>%
  mutate(column = str_replace_all(column, "L", "0")) %>%
  mutate(column = str_replace_all(column, "R", "1")) %>%
  mutate(row = str_split(row, boundary("character"))) %>%
  mutate(column = str_split(column, boundary("character"))) %>%
  rowwise() %>%
  mutate(row_dec = as_decimal(row)) %>%
  mutate(column_dec = as_decimal(column)) %>%
  mutate(seat_id = (row_dec*8)+column_dec)
# Part 1
print(max(data$seat_id))
# Part 2
compare <- tibble(seat_id=1:max(data$seat_id))
compare <- compare[-data$seat_id,]
print(compare)