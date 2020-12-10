library(tidyverse)

### PART 1

valid_passports <- function(passport_df) {
  ev <- c("byr", "iyr", "eyr","hgt","hcl","ecl","pid")

  passport_df %>%
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
}

passports_text_example <- read_file("data/4-example.txt")
valid_passports(passports_text_example)
# [1] 2

passports_text <- read_file("data/4.txt")
valid_passports(passports_text)
# [1] 204
