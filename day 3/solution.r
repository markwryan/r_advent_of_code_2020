install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(readr)

find_trees <- function(run, rise) {
  row <- 1
  column <- 1
  trees <- 0
  # Hard-coded total number of rows
  while(row <= 323) {
    if(data[row,column] == "#") {
      trees <- trees + 1
    }
    
    row <- row + rise
    column <- column + run
    # Hard-coded total number of columns
    if(column > 31) {
      column <- column - 31
    }
  }
  return(trees)
}

# Probably a better way to do this, but read in data as fixed-width values, each 1 char long
data <- read_fwf("input.txt", fwf_widths(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))

# Part 1
print(find_trees(3,1))

# Part 2
total_trees <- find_trees(1,1) * find_trees(3,1) * find_trees(5,1) * find_trees(7,1) * find_trees(1,2)
print(total_trees)
