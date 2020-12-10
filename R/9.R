library(tidyverse)

invalid_xmas <- function(xs, preamble) {
  xs_i <- suppressWarnings((as.numeric(xs)))
  pa <- suppressWarnings(as.integer(preamble))

  stopifnot(length(xs_i) > 1, !any(is.na(xs_i)),
            is.numeric(pa), pa >= 0)

  xmas_df <- tibble(xmas_seq = xs_i)

  xmas_df[["front"]] <- seq_along(xs_i) - 1
  xmas_df[["back"]] <- vapply(xmas_df[["front"]],
                              function(x) if (x >= (pa)) x - (pa - 1) else NA_integer_,
                              1)

  .vector_pair_sums <- function(v) {
    if (length(v) > 0) {
      combn_matrix <- gtools::combinations(length(v), 2, v)
      unique(rowSums(combn_matrix))
    } else {
      0
    }
  }

  out_df <- xmas_df %>%
    mutate(is_preamble = is.na(back),
           prev_n = map2(back, front,
                        ~ if (!is.na(.x)) xs_i[.x:.y] else numeric()),
          prev_n_sums = map(prev_n, ~ .vector_pair_sums(.x)),
          valid_xmas = map2_lgl(xmas_seq, prev_n_sums, ~ .x %in% .y))

  invalid_value <- out_df %>%
    filter(!is_preamble & !valid_xmas) %>%
    pull(xmas_seq)

  list("df" = out_df, "invalid_value" = invalid_value)

}

xmas <- c(5, 20, 15, 25, 47, 40, 62, 55, 65, 95,
          102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

invalid_xmas(xmas, 5)[["invalid_value"]]
# [1] 127

xmas_full <- readr::read_delim("./data/9.txt",
  delim = "\n",
  col_names = FALSE,
  col_types = cols(
    X1 = col_double()
  )
  ) %>%
  pull(X1)

invalid_xmas_full <- invalid_xmas(xmas_full, 25)

xmas_full_invalid_value <- invalid_xmas_full[["invalid_value"]]
xmas_full_invalid_value
# [1] 1721308972

### PART 2
 
find_first_target_value <- function(df, target) {
  stopifnot(is.numeric(target), is.data.frame(df))

  match_rowid <- df %>%
                  mutate(cs = cumsum(xmas_seq)) %>%
                  filter(cs == target) %>%
                  pull(rowid) %>%
                  subset(., !is.na(.))

  if (length(match_rowid) < 1) {
    return(NA_integer_)
  } else {
    return(min(match_rowid, na.rm = TRUE))
  }
}

xmas_full_df <- invalid_xmas_full[["df"]] %>%
  rowid_to_column() %>%
  mutate(range_candidate = xmas_seq < xmas_full_invalid_value)

last_candidate <- xmas_full_df %>%
  filter(range_candidate) %>%
  pull(rowid) %>%
  max()

xmas_full_invalid_row <- xmas_full_df %>%
  filter(xmas_seq == xmas_full_invalid_value) %>%
  pull(rowid)

xmas_full_soln_df <- xmas_full_df %>%
    filter(rowid < min(last_candidate, xmas_full_invalid_row)) %>%
    select(rowid, xmas_seq)

xmas_full_soln_df %>%
  mutate(following_df = map(rowid, ~ filter(xmas_full_soln_df, rowid >= .x)),
         matching_value_rowid =
          map_int(following_df,
                  ~ find_first_target_value(.x, xmas_full_invalid_value))) %>%
  filter(!is.na(matching_value_rowid)) %>%
  select(-following_df) %>%
  mutate(contiguous_vector = map2(rowid, matching_value_rowid,
                                  ~ xmas_full[.x:.y]),
         min_cv_value = min(unlist(contiguous_vector), na.rm = TRUE),
         max_cv_value = max(unlist(contiguous_vector), na.rm = TRUE),
         encryption_weakness = min_cv_value + max_cv_value) %>%
  pull(encryption_weakness)
# [1] 209694133
