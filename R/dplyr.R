
# issues related to bug in https://github.com/tidyverse/dplyr/issues/4096

group_by_drop <- function(...) {
  group_by(..., .drop = TRUE)
}
