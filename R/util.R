`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
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
