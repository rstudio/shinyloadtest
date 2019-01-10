
# issues related to bug in https://github.com/tidyverse/dplyr/issues/4096


convert_label_to_chr <- function(df) {
  vars <- df %>% group_vars()
  if (length(vars) > 0) {
    df %>%
      ungroup() %>%
      mutate(label = as.character(label)) %>%
      group_by_(vars)
  } else {
    df %>%
      mutate(label = as.character(label))
  }
}


convert_run_to_chr <- function(df) {
  vars <- df %>% group_vars()
  if (length(vars) > 0) {
    df %>%
      ungroup() %>%
      mutate(run = as.character(run)) %>%
      group_by_(vars)
  } else {
    df %>%
      mutate(run = as.character(run))
  }
}

convert_run_to_fctr <- function(df, original_df) {
  vars <- df %>% group_vars()
  if (length(vars) > 0) {
    df %>%
      ungroup() %>%
      mutate(run = factor(run, levels = levels(original_df$run))) %>%
      group_by_(vars)
  } else {
    df %>%
      mutate(run = factor(run, levels = levels(original_df$run)))
  }
}
