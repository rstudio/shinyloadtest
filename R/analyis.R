# Utility functions -------------------------------------------------------

strip_suffix <- function(str) {
  sub("(.*)_.*", "\\1", str)
}


# Read a single .log file
read_log_file <- function(file) {
  sess <- basename(file) %>% sub(".log$", "", .) %>% as.integer()
  suppressWarnings({
    df <- readr::read_csv(file, col_types = "icdic", comment = "#")
  })
  tibble::as.tibble(cbind(df, data.frame(session = sess, stringsAsFactors = FALSE)))
}

# Read a "sessions/" directory full of .log files
read_log_dir <- function(dir, name = basename(dirname(dir))) {
  files <- list.files(dir, pattern = "*.log", full.names = TRUE)
  df <- lapply(files, read_log_file) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(timestamp = (timestamp - min(timestamp)) / 1000)

  relative_concurrency <- with(df, {
    ifelse(event == "WS_OPEN_START", 1,
           ifelse(event == "WS_CLOSE_END", -1,
                  0))
  })
  df <- df %>%
    dplyr::mutate(concurrency = cumsum(relative_concurrency),
           run = name)

  df
}

# Read a recording fil
read_recording <- function(recording) {

}

get_times <- function(df) {
  df %>%
    dplyr::filter(!is.na(input_line_number)) %>%
    dplyr::group_by(run, session, input_line_number) %>%
    dplyr::summarise(event = strip_suffix(event[1]),
              start = min(timestamp),
              end = max(timestamp),
              time = diff(range(timestamp)),
              concurrency = mean(concurrency)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(input_line_number, run) %>%
    tibble::as.tibble()
}



# Tidying functions -------------------------------------------------------

#' Create Tidy Load Test Results
#' @description The \code{shinycannon} tool creates a directory of log files for
#'   each load test. This function translates one or more test result
#'   directories into a tidy data frame.
#' @param ...  Key-value pairs where the key is the desired name for the test and the
#'   value is a path to the test result directory.
#'
#' @return A tidy data frame with the test result data. Each row is an event. Columns include
#'    identifiers and timing information for the event.
#' @export
#' @usage
#'   tidy_loadtest(
#'      test1 = 'results/test-1/',
#'      test2 = 'results/test-2/'
#'   )
#'
tidy_loadtest <- function(...) {

  # TODO: Validate input directories and fail intelligently!

  run_levels <- names(list(...))

  df <- list(...) %>%
    lapply(file.path, "sessions") %>%
    { mapply(., FUN = read_log_dir, names(.), USE.NAMES = FALSE, SIMPLIFY = FALSE) } %>%
    lapply(get_times) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(event != "PLAYBACK_SLEEPBEFORE") %>%
    dplyr::arrange(input_line_number, run) %>%
    dplyr::mutate(label = paste0(input_line_number, ":", event))

  fct_levels <- df$input_line_number[!duplicated(df$input_line_number)]
  fct_labels <- df$label[!duplicated(df$input_line_number)]
  df <- df %>%
    dplyr::mutate(label = factor(input_line_number, fct_levels, fct_labels, ordered = TRUE)) %>%
    dplyr::mutate(run = factor(run, run_levels, ordered = TRUE))

  df
}

