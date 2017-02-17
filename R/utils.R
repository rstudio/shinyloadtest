dir_exists <- function(path) utils::file_test('-d', path)

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && as.integer(x) == x
}
