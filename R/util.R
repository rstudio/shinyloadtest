# Bind names to values using named arguments, then evaluate expr in that environment
# Discards the return value of expr and returns the value of the last binding
# Example: with_let(y = 10, x = 1, cat("x and y are", x, "and", y, "\n")) + 123
# Prints message and evaluates to 124
with_let <- function(expr, ...) {
  bindings <- rlang::list2(...)
  stopifnot(rlang::is_named(bindings))
  stopifnot(length(bindings) > 0)
  rlang::eval_tidy(rlang::enquo(expr), bindings)
  bindings[[length(bindings)]]
}

# Returns a new vector of new_length, starting with the elements of vec,
# followed by the last element of vec repeated as many times as necessary.
fill_to <- function(vec, new_length) {
  if (length(vec) == new_length) return(vec)
  if (length(vec) == 0 || new_length < length(vec))
    stop("Vector is length 0 or new_length < vector length")
  append(vec, rep(vec[[length(vec)]], new_length-length(vec)))
}

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}
