`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x))) {
    y
  } else {
    x
  }
}


glue_file <- function(file, data, ...) {
  assert_is_available("glue")
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

fct_rev <- function(x) {
  factor(x, levels = rev(unique(x)))
}

str_trunc <- function(x, width = 100) {
  x <- as.character(x)

  too_long <- nchar(x) > width
  x[too_long] <- paste0(substr(x[too_long], 1, width - 3), "...")
  x
}

`%||%` <- function(a, b) if (is.null(a)) b else a


is_available <- function(package, version = NULL) {
  installed <- nzchar(system.file(package = package))
  if (is.null(version)) {
    return(installed)
  }
  installed && isTRUE(utils::packageVersion(package) >= version)
}

assert_is_available <- function(package, version = NULL) {
  if (!is_available(package, version)) {
    cli::cli_abort(paste0(
      "Please install the `", package, "` package and try again."
    ))
  }
}
