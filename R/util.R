`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

# Returns a list of lists, each a path into the nested structure x. Each path
# ends with the value at the leaf.
paths <- function(parent, x) {
  paths_recurse <- function(items) {
    unlist(
      lapply(items, function(item) {
        paths(c(parent, item), x[[item]])
      }),
      recursive = FALSE
    )
  }
  if (typeof(x) == "list") {
    if (is.null(names(x))) {
      # Unnamed list aka JSON array
      paths_recurse(seq_along(x))
    } else if (length(x) > 0) {
      # Named list aka JSON object
      paths_recurse(names(x))
    }
  } else {
    # Leaf, return the path to the leaf and its value
    list(c(parent, x))
  }
}

# Returns a data frame where each row is a distinct path into a tree of lists,
# such as that produced by jsonlite::fromJSON. Useful for querying deeply-nested
# structures in a tabular fashion.
tree_df <- function(tree) {
  ps <- paths(list(), tree)

  # Ensure rows are all the same length, with NA for padding
  lengths <- vapply(ps, length, numeric(1))
  longest <- max(lengths)
  padded <- Map(ps, lengths, f = function(path, len) {
    append(path, rep(NA, longest - len))
  })

  as.data.frame(do.call(rbind, padded))
}

glue_file <- function(file, data, ...) {
  glue::glue_data(
    data,
    paste0(readLines(file), collapse = "\n"),
    ...
  )
}

glue_inst_file <- function(file, data, ...) {
  glue_file(system.file(file, package = "shinyloadtest"), data, ...)
}

glue_component <- function(file, data, ...) {
  glue_inst_file(file.path("dist", "components", paste0(file, ".html")), data, ...)
}

glue_multi_component <- function(file, datas, ..., collapse = "") {
  ret <- vapply(datas, glue_component, file = file, ..., FUN.VALUE = character(1))
  paste0(ret, collapse = collapse)
}

glue_index <- function(data, ...) {
  glue_component("index", data, ...)
}
