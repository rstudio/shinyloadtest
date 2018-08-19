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


if (FALSE) {

  svg_file <- function(file) {
    system.file(file.path("dist", "SVG", paste0(file, ".svg")), package = "shinyloadtest")
  }

  gantt <- list(
    list(n = 1, src = svg_file("4workerbaseline"), id = "one-user-chart", data_view = "one-user-chart", name = "1 User"),
    list(n = 4, src = svg_file("4workerbaseline"), id = "four-user-chart", data_view = "four-user-chart", name = "4 Users"),
    list(n = 16, src = svg_file("16workerbaseline"), id = "sixteen-user-chart", data_view = "sixteen-user-chart", name = "16 Users")
  )
  boxplot <- list(
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot")),
    list(src = svg_file("boxplot"))
  )

dev_load()
  glue_index(
    list(
      gantt = gantt,
      boxplot = boxplot
    )
  )

  withr::with_dir("~/rstudio/shinycannon/shinycannon", {
    df <- tidy_loadtest(
      `1 user` = "./demo1",
      `4 users` = "./demo4",
      `16 users` = "./demo16"
    )
  })


  dev_load()
  make_report(df)

}
