`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}


glue_file <- function(data, file, ...) {
  glue::glue_data(
    data,
    paste0(readLines(file), collapse = "\n"),
    ...
  )
}

glue_inst_file <- function(data, file, ...) {
  glue_file(data, system.file(file, package = "shinyloadtest"), ...)
}

glue_component <- function(data, file, ...) {
  glue_inst_file(data, file.path("dist", "components", file), ...)
}
