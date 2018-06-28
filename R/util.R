`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

# Like Map but expects f to return a list and concatenates results.
Mapcan <- function(f, xs) Reduce(append, Map(f, xs))

# Returns a list of named lists, each with `name` and `value` values, for the
# given named list.
as_entries <- function(named_list) {
  Map(function(name) list(key = name, val = named_list[[name]]), names(named_list))
}

# Returns a list of lists, each a path into the nested structure x. Each path
# ends with the value at the leaf.
paths <- function(parent, x) {
  if (typeof(x) == "list") {
    # Named list aka JSON object
    if (!is.null(names(x))) {
      Mapcan(function(entry) {
        paths(c(parent, entry$key), entry$val)
      }, as_entries(x))
    # Unnamed list aka JSON array
    } else if (length(x) > 0) {
      Mapcan(function(i) {
        paths(c(parent, i), x[[i]])
      }, 1:length(x))
    }
  } else {
    # Leaf, so return the path here + the leaf value
    list(c(parent, x))
  }
}

# Returns a data frame where each row is a distinct path into a tree of lists,
# such as that produced by jsonlite::fromJSON. Useful for querying deeply-nested
# structures in a tabular fashion.
tree_df <- function(tree) {
  ps <- paths(list(), tree)

  # Ensure rows are all the same length, with NA for padding
  longest <- max(unlist(Map(length, ps)))
  padded <- Map(function(path) {
    c(as.list(path), rep(NA, longest-length(path)))
  }, ps)

  do.call(rbind, padded)
}
