if (getRversion() >= "2.15.1") {
  # TODO remove and upgrade the dplyr fns to FN_()
  utils::globalVariables(c("input_line_number", "run", "session_id", "user_id", "iteration", "event", "timestamp", "concurrency", "center", "event_class", "total_latency", ".", "type"))
}

cutoffColor <- "red"

# Utility functions -------------------------------------------------------

strip_suffix <- function(str) {
  sub("(.*)_.*", "\\1", str)
}


# Read a single .log file
read_log_file <- function(file) {
  suppressWarnings({
    df <- readr::read_csv(
      file,
      col_types = readr::cols(
        session_id = readr::col_integer(),
        worker_id = readr::col_integer(),
        iteration = readr::col_integer(),
        event = readr::col_character(),
        timestamp = readr::col_double(),
        input_line_number = readr::col_integer(),
        comment = readr::col_character()
      ),
      comment = "#"
    )
  })
  df %>%
    mutate(user_id = worker_id) %>%
    select(- worker_id)
}

# Read a "sessions/" directory full of .log files
read_log_dir <- function(dir, name = basename(dirname(dir))) {
  files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
  if (length(files) == 0) {
    stop("No files found for dir: ", dir)
  }
  df <- lapply(files, read_log_file) %>%
    bind_rows() %>%
    arrange(timestamp) %>%
    mutate(timestamp = (.$timestamp - min(.$timestamp)) / 1000)

  relative_concurrency <- with(df, {
    ifelse(event == "WS_OPEN_START", 1,
           ifelse(event == "WS_CLOSE_END", -1,
                  0))
  })
  df <- df %>%
    mutate(
      concurrency = cumsum(relative_concurrency),
      run = name
    )

  df
}

# Read a recording file
read_recording <- function(fileName) {
  baselineInfo <- fileName %>%
    readLines() %>%
    lapply(jsonlite::fromJSON) %>%
    lapply(function(item) {
      item$begin <- lubridate::as_datetime(item$begin)
      if (!is.null(item$end)) item$end <- lubridate::as_datetime(item$end)
      item
    })
  startTime <- baselineInfo[[1]]$begin
  baselineInfo %>%
    lapply(function(info) {
      tibble::tibble(
        event = info$type,
        start = as.numeric(difftime(info$begin, startTime, units = "secs")),
        end = if (!is.null(info$end))
            as.numeric(difftime(info$begin, startTime, units = "secs"))
          else
            as.numeric(difftime(info$begin, startTime, units = "secs")) + 0.25,
        json = list(info),
      )
    }) %>%
    bind_rows() %>%
    mutate(
      session_id = -1,
      user_id = -1,
      iteration = -1,
      input_line_number = seq_len(nrow(.)),
      time = end - start,
      concurrency = 1,
      label = recording_item_labels(json)
    ) %>%
    arrange(input_line_number) %>%
    mutate(label = factor(label, levels = label, ordered = TRUE)) %>%
    select(
      # session_id, user_id, iteration,
      input_line_number,
      event,
      start, end, time,
      # concurrency,
      label,
      json
    )
}

recording_item_labels <- function(x_list) {
  shorten_url <- function(u) {
    if (grepl("^/[^/]+/[^/]+/", u)) {
      parts <- strsplit(u, "/")[[1]]
      paste0(".../", parts[length(parts)])
    } else {
      u
    }
  }
  ret <- c()
  input_changes <- c()

  prepend_line <- function(a, i) {
    paste0(i, ") ", a)
  }

  for (i in seq_along(x_list)) {
    x <- x_list[[i]]

    new_label <- switch(x$type,
      "REQ_HOME" = "Get: Homepage",
      "REQ_GET" = paste0("Get: '", shorten_url(x$url), "'"),
      "WS_OPEN" = "<Open Websocket>",
      "WS_RECV_INIT" = "<Initialize Websocket>",
      "WS_SEND" = {
        if (i > 1 && identical(x_list[[i - 1]]$type, "WS_RECV_INIT")) {
          "<Initialize Inputs>"
        } else {
          message <- jsonlite::fromJSON(x$message)
          name_vals <- names(message$data)
          visible_name_vals <- name_vals[!grepl("^\\.", name_vals)]
          paste0("Set: ", paste0(visible_name_vals, collapse = ", "))
        }
      },
      "WS_RECV" = {
        message <- jsonlite::fromJSON(x$message)
        paste0("Updated: ", paste0(names(message$values), collapse = ", "))
      },
      "WS_CLOSE" = "<Close Websocket>",
      prepend_line(x$type)
    )
    ret <- append(ret, prepend_line(new_label, i))
  }
  ret
}

get_times <- function(df) {
  df %>%
    filter(!is.na(input_line_number)) %>%
    group_by(run, session_id, user_id, iteration, input_line_number) %>%
    summarise(event = strip_suffix(event[1]),
              start = min(timestamp),
              end = max(timestamp),
              time = diff(range(timestamp)),
              concurrency = mean(concurrency)) %>%
    ungroup() %>%
    arrange(input_line_number, run) %>%
    tibble::as.tibble()
}



# Tidying functions -------------------------------------------------------

#' Create Tidy Load Test Results
#' @description The \code{shinycannon} tool creates a directory of log files for
#'   each load test. This function translates one or more test result
#'   directories into a tidy data frame.
#' @param ...  Key-value pairs where the key is the desired name for the test and the
#'   value is a path to the test result directory.
#' @return A tidy data frame with the test result data. Each row is an event. Columns include
#'    identifiers and timing information for the event.
#' @export
#' @examples
#' \dontrun{
#'   tidy_loadtest(
#'      `1 core` = 'results/test-1/',
#'      `2 cores` = 'results/test-2/'
#'   )
#' }
tidy_loadtest <- function(..., verbose = TRUE) {

  # TODO: Validate input directories and fail intelligently!
  verbose <- isTRUE(verbose)

  run_levels <- names(list(...))

  df <- list(...) %>%
    {
      mapply(
        ., names(.),
        USE.NAMES = FALSE, SIMPLIFY = FALSE,
        FUN = function(recording_path, run) {
          if (verbose) message("Processing: '", run, "'")
          df_run <- read_log_dir(file.path(recording_path, "sessions"), run) %>%
            get_times() %>%
            filter(event != "PLAYBACK_SLEEPBEFORE", event != "PLAYER_SESSION") %>%
            arrange(run, user_id, session_id, input_line_number) %>%
            mutate(
              label = paste0(input_line_number, ":", event),
              type = "record"
            ) %>%
            select(run, type, everything())

          df_recording <- read_recording(file.path(recording_path, "recording.log")) %>%
            mutate(
              recording_label = label
            ) %>%
            select(input_line_number, recording_label)

          left_join(df_run, df_recording, by = "input_line_number")
        }
      )
    } %>%
    bind_rows()

  fct_levels <- df$input_line_number[!duplicated(df$input_line_number)]
  fct_labels <- df$label[!duplicated(df$input_line_number)]
  df <- df %>%
    mutate(
      label = factor(input_line_number, fct_levels, fct_labels, ordered = TRUE),
      run = factor(run, run_levels, ordered = TRUE)
    )

  df$id <- seq_len(nrow(df))
  df_filtered <- filter_df(df)

  df <- df %>%
    ungroup() %>%
    mutate(
      maintenance = id %in% df_filtered$id
    )

  df
}



filter_df_start_time <- function(df) {
  df_start <- df %>%
    group_by(user_id) %>%
    summarise(start_time = min(start))
  max(df_start$start_time)
}
filter_df_end_time <- function(df) {
  df_end <- df %>%
    group_by(user_id) %>%
    summarise(end_time = max(end))
  min(df_end$end_time)
}
filter_df <- function(
  df,
  start_time,
  end_time
) {
  is_missing_start_time <- missing(start_time)
  is_missing_end_time <- missing(end_time)
  lapply(
    unique(df$run),
    function(runVal) {

      df_run <- df %>% filter(run == runVal)

      if (is_missing_start_time) start_time <- filter_df_start_time(df_run)
      if (is_missing_end_time) end_time <- filter_df_end_time(df_run)

      df_times <- df_run %>%
        group_by(session_id) %>%
        summarise(min_start = min(start), max_end = max(end)) %>%
        filter(min_start >= start_time, max_end <= end_time)

      df_run %>%
        ungroup() %>%
        filter(session_id %in% df_times$session_id)
    }
  ) %>%
    bind_rows()
}
