install.packages("tidyverse")
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(jsonlite)

# Diversify your bonds, anchor your REGEX
has_required <- function(entry) {
  return (
        str_detect(entry["byr"],"^\\d{4}$") &
        as.numeric(entry["byr"]) >= 1920 &
        as.numeric(entry["byr"]) <= 2002 &
        str_detect(entry['iyr'], "\\d{4}") &
        as.numeric(entry["iyr"]) >= 2010 &
        as.numeric(entry["iyr"]) <= 2020 &
        str_detect(entry["eyr"], "\\d{4}") &
        as.numeric(entry["eyr"]) >= 2020 &
        as.numeric(entry["eyr"]) <= 2030 &
        str_detect(entry["hgt"], "(\\d{3}cm)|(\\d{2}in)") &
        str_detect(entry["hcl"], "^#[0-9|a-f]{6}$") &
        str_detect(entry["ecl"],"^(amb|blu|brn|grn|gry|hzl|oth)$") &
        str_detect(entry["pid"], "^\\d{9}$")
      )        
}

data <- read_file("input.txt") %>%
  str_replace_all("\r\n\r\n", "\"},{\"") %>%
  str_replace_all("\r\n","\",\"") %>%
  str_replace_all(" ","\",\"") %>%
  str_replace_all(":","\":\"")

data <- str_c("[{\"",data, "\"}]") %>%
  fromJSON()

# Part 1
data <- data %>%
  drop_na(byr) %>%
  drop_na(iyr) %>%
  drop_na(eyr) %>%
  drop_na(hgt) %>%
  drop_na(hcl) %>%
  drop_na(ecl) %>%
  drop_na(pid)
  

# Part 2
valid <- NULL
for(i in 1:nrow(data)) {
  entry <- data[i,]
  if(has_required(entry)) {
    if(str_detect(entry["hgt"], "\\d{3}cm")) {
      hgt <- as.numeric(str_sub(entry["hgt"],1,3))
      if(hgt >= 150 & hgt <= 193) {
        valid <- rbind(valid, entry)
      }
    }
    if(str_detect(entry["hgt"], "\\d{2}in")) {
      hgt <- as.numeric(str_sub(entry["hgt"],1,2))
      if(hgt >= 59 & hgt <= 76) {
        valid <- rbind(valid, entry)
      }
    }
  }
}

