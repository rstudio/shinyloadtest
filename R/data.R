# Generates a single roxygen tag. For use either in documenting example data,
# or any function that returns a df in the shape of the one returned by load_runs()
# TODO de-dupe with description in vignettes/analyzing-load-test-logs.Rmd
roxygen_gen_df_desc <- function(first_line, has_json) {
  stopifnot(startsWith(first_line, "@"))
  c(
    first_line,
    "\\describe{",
    " \\item{run}{Name of the run}",
    " \\item{session_id}{simulated session identifier, 0-based}",
    " \\item{user_id}{simulated user identifier, 0-based}",
    " \\item{iteration}{user session identifier, 0-based}",
    " \\item{input_line_number}{recording line number associated with event}",
    " \\item{event}{type of the event}",
    " \\item{start}{time the event started, in seconds, relative to the time at which all simulated users were running.}",
    " \\item{end}{time the event ended, in seconds, relative to the time at which all simulated users were running}",
    " \\item{time}{event duration, in seconds}",
    " \\item{concurrency}{number of events that happened at the same time as this one}",
    " \\item{maintenance}{whether this event occurred before or after all simulated users were running}",
    " \\item{label}{event-specific text label}",
    if (has_json) "\\item{json}{raw message JSON and parsed JSON of the event}",
    "}"
  )
}

# Generates an entire roxygen block for any example data .rda.
roxygen_gen_demo <- function(rda_file) {
  env <- new.env(parent = emptyenv())
  load(rda_file, envir = env)
  var_name <- tools::file_path_sans_ext(basename(rda_file))
  df <- env[[var_name]]
  users <- max(df[["user_id"]]) + 1
  rows <- nrow(df)
  cols <- ncol(df)
  c(
    sprintf("Example metrics for a %s-user load test", users),
    "",
    sprintf("An example dataset like that returned by \\code{\\link{load_runs}}, but without the \\code{json} variable for portability. Contains latency data for %s \\code{shinycannon} events suitable for passing to \\code{\\link{shinyloadtest_report}}.", rows),
    "",
    roxygen_gen_df_desc(sprintf("@format A data frame with %s rows and %s variables:", rows, cols), FALSE)
  )
}

#' @eval roxygen_gen_demo("data/slt_demo_data_1.rda")
"slt_demo_data_1"

#' @eval roxygen_gen_demo("data/slt_demo_data_4.rda")
"slt_demo_data_4"

#' @eval roxygen_gen_demo("data/slt_demo_data_16.rda")
"slt_demo_data_16"
