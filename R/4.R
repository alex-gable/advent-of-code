library(tidyverse)

passports_text <- c("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

ev <- c("byr", "iyr", "eyr","hgt","hcl","ecl","pid")

passports_text %>%
  str_split("\n\n") %>%
  pluck(1) %>%
  str_replace_all("\n", " ") %>%
  str_split(" ") %>%
  map_depth(2, ~ str_split(.x, ":") %>% unlist()) %>%
  map_depth(2, ~ unlist(.x)) %>%
  map_depth(2, ~ .x[1]) %>%
  map(~ str_c(.x)) %>%
  map(~ all(ev %in% .x)) %>%
  unlist() %>%
  sum()