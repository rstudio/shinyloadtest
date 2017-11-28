dir_exists <- function(path) utils::file_test('-d', path)

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && as.integer(x) == x
}

is_valid_rmd_name <- function(x) {
  valid <- grepl("^[a-zA-Z0-9_-]+\\.[Rr]md$", x)
  if (valid) {
    TRUE
  } else {
    stop(paste0(x, " is not a valid filename. filename must include .Rmd extension"))
  }
}
