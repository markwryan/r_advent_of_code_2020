library(tidyr)
library(dplyr)
library(readr)
library(stringr)

data <- read_file("input.txt") %>%
  str_replace_all("(?<!\n)\n(?!\n)",",") %>%
  str_replace_all("\n\n","\n") %>%
  read_csv(col_names = FALSE) %>%
  unite("answers",c("X1","X2","X3","X4","X5"), remove = FALSE, na.rm = TRUE, sep = "") %>%
  mutate(answers = unique(str_split(answers, boundary("character"))))

# Part 1
data$part1_total <- 0
for(i in 1:nrow(data)) {
  data$answers[[i]] <- unique(data$answers[[i]])
  data$total[i] <- length(data$answers[[i]]) 
}
print(sum(data$total))

# Part 2
part2 <- tibble(data[2:6])
part2$count <- 0
part2$total <- 0
for(i in 1:nrow(part2)) {
  for(j in 1:5) {
    if(!is.na(part2[i,j])) {
      part2[i,6]<- part2[i,6] + 1
    }
  }
  for(letter  in letters) {
    match <- TRUE
    for(x in 1:as.numeric(part2[i,6])) {
      if(!str_detect(part2[i,x], letter)) {
        match <- FALSE
      }
    }
    if(match) {
      part2[i,7] <- part2[i,7] + 1
    }
  }
}
print(sum(part2$total))
