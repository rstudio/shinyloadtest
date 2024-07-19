# TODO remove and upgrade the dplyr fns to FN_()
utils::globalVariables(c("input_line_number", "run", "session_id", "user_id", "iteration", "event", "timestamp", "concurrency", "center", "event_class", "total_latency", ".", "type", "min_start", "max_end", "worker_id", "json", "eventLabel", "duration"))

# Utility functions -------------------------------------------------------

strip_suffix <- function(str) {
  sub("(.*)_.*", "\\1", str)
}


# Read a "sessions/" directory full of .log files
read_log_dir <- function(dir, name = basename(dirname(dir)), verbose = vroom::vroom_progress()) {
  files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
  if (length(files) == 0) {
    cli::cli_abort(paste0("No files found for run dir: ", dir), call = NULL)
  }

  df <- vroom::vroom(files,
    col_types = vroom::cols(
      session_id = vroom::col_integer(),
      worker_id = vroom::col_integer(),
      iteration = vroom::col_integer(),
      event = vroom::col_character(),
      timestamp = vroom::col_double(),
      input_line_number = vroom::col_integer(),
      comment = vroom::col_character()
    ),
    comment = "#",
    progress = vroom::vroom_progress()
  )

  df <- df %>%
    arrange(timestamp) %>%
    mutate(
      timestamp = (timestamp - min(timestamp)) / 1000,
      concurrency = cumsum(case_when(
        event == "WS_OPEN_START" ~ 1,
        event == "WS_CLOSE_END" ~ -1,
        TRUE ~ 0
      )),
      run = name
    ) %>%
    rename(user_id = worker_id)
  df
}

# Read a recording file
read_recording <- function(file_name) {
  assert_is_available("lubridate")

  file_lines <- readLines(file_name)
  input_line_number <- seq_along(file_lines)
  not_comments <- (!grepl("^#", file_lines))
  file_lines <- file_lines[not_comments]
  input_line_number <- input_line_number[not_comments]
  baselineInfo <- file_lines %>%
    lapply(jsonlite::fromJSON) %>%
    lapply(function(item) {
      item$begin <- lubridate::as_datetime(item$begin)
      if (!is.null(item$end)) item$end <- lubridate::as_datetime(item$end)
      item
    })
  startTime <- baselineInfo[[1]]$begin
  baselineData <- baselineInfo %>%
    lapply(function(info) {
      tibble(
        event = info$type,
        start = as.numeric(difftime(info$begin, startTime, units = "secs")),
        end = if (!is.null(info$end)) {
          as.numeric(difftime(info$end, startTime, units = "secs"))
        } else {
          as.numeric(difftime(info$begin, startTime, units = "secs")) + 0.001
        },
        json = list(info),
      )
    }) %>%
    bind_rows()

  baselineData %>%
    mutate(
      json = lapply(json, function(json_item) {
        if (!is.null(json_item$message) && !identical(json_item$message, "o")) {
          json_item$message_parsed <- parseMessage(json_item$message)
        } else {
          json_item$message_parsed <- list()
        }
        json_item
      })
    ) %>%
    mutate(
      input_line_number = input_line_number,
      time = end - start,
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

# A WS message with this payload is only ever sent when SockJS is used
# (SSP/RSC). The message may begin with the following strings:
#
# 1. "[\"0|o|": This type of message is sent when an older version of SSP is
# used that doesn't support subapps. The 0 here is the SockJS message id.
#
# 2. "[\"0#<subapp-id>|o|": This type of message is sent with more recent
# versions of SSP, and with RSC. It indicates that the server supports subapps.
# <subapp-id> is an integer identifying the subapp. For single applications, it
# is 0. It is the same for the duration of the session. We have not tested with
# subapps and so it is unknown as of this writing whether or not shinyloadtest
# works with them.
isSockjsInitMessage <- function(msg) {
  grepl("^\\[\"0(#\\d+)?\\|o\\|", msg)
}

WS_CLOSE_LABEL <- "Stop Session"
recording_item_labels <- function(x_list) {
  shorten_url <- function(u) {
    parts <- strsplit(u, "/")[[1]]
    parts[length(parts)]
  }
  ret <- c()
  input_changes <- c()

  prepend_line <- function(a, i) {
    paste0("Event ", i, ") ", a)
  }

  for (i in seq_along(x_list)) {
    x <- x_list[[i]]

    new_label <- switch(x$type,
      "REQ_HOME" = "Get: Homepage",
      "REQ_GET" = paste0("Get: ", shorten_url(x$url)),
      "REQ_TOK" = "Get: Shiny Token",
      "REQ_SINF" = "Get: Connection Information",
      "REQ_POST" = "Post Request",
      "WS_RECV_BEGIN_UPLOAD" = "File Upload",
      "WS_OPEN" = "Start Session",
      "WS_RECV_INIT" = "Initialize Session",
      "WS_SEND" = {
        if (i > 1 && identical(x_list[[i - 1]]$type, "WS_RECV_INIT")) {
          "Initialize Inputs"
        } else {
          if (isSockjsInitMessage(x$message)) {
            "SockJS Initialize Connection"
          } else {
            message <- x$message_parsed
            if (identical(message$method, "uploadInit")) {
              "Start File Upload"
            } else if (identical(message$method, "uploadEnd")) {
              "Stop File Upload:"
            } else {
              name_vals <- names(message$data)
              visible_name_vals <- name_vals[!grepl("^\\.", name_vals)]
              paste0("Set: ", paste0(visible_name_vals, collapse = ", "))
            }
          }
        }
      },
      "WS_RECV" = {
        if (x$message == "o") {
          "Start Connection"
        } else {
          message <- x$message_parsed
          if (!is.null(message$response$tag)) {
            "Completed File Upload"
          } else {
            paste0("Updated: ", paste0(names(message$values), collapse = ", "))
          }
        }
      },
      "WS_CLOSE" = WS_CLOSE_LABEL,
      x$type
    )
    ret <- append(ret, prepend_line(new_label, i))
  }
  ret
}

get_times <- function(df) {
  df %>%
    filter(
      !is.na(input_line_number),
      event != "PLAYER_SESSION_CREATE",
      !grepl("^PLAYBACK", event) # remove all playback_** events
    ) %>%
    group_by(run, user_id, session_id, iteration, input_line_number) %>%
    summarise(
      event = strip_suffix(event[[1]]),
      start = min(timestamp),
      end = max(timestamp),
      time = diff(range(timestamp)),
      concurrency = mean(concurrency),
      .groups = "drop"
    )
}



# Tidying functions -------------------------------------------------------

#' Create Tidy Load Test Results
#' @description The `shinycannon` tool creates a directory of log files for
#'   each load test. This function translates one or more test result
#'   directories into a tidy data frame.
#' @section Output variables:
#'
#' * `run`: The name of the recording session.
#' * `session_id`: An incrementing integer value for every session within
#'    a `run`. Starts at 0.
#' * `user_id`: Which simulated user is performing the work within a `run`.
#'    Starts at 0.
#' * `iteration`: an incrementing integer value of the session iteration
#'    for the #' matching `user_id`. Starts at 0.
#' * `input_line_number`: The line number corresponding to the event in the
#'   `recording.log` file.
#' * `event`: the web event being performed. One of the following values:
#'     * `REQ_HOME`: initial request for to load the homepage
#'     * `REQ_GET`: Request a supporting file (JavaScript / CSS)
#'     * `REQ_TOK`: Request a Shiny token
#'     * `REQ_SINF`: Request SockJS information
#'     * `REQ_POST`: Perform a POST query, such as uploading part of a file
#'     * `WS_RECV_BEGIN_UPLOAD`: A file upload is being requested
#'     * `WS_OPEN`: Open a new SockJS connection
#'     * `WS_RECV_INIT`: Initialize a new SockJS
#'     * `WS_SEND`: Send information from the Shiny server to the browser
#'     * `WS_RECV`: Send information from the browser to the Shiny server
#'     * `WS_CLOSE`: Close the SockJS connection
#' * `start`: Start time of the event relative to the beginning of the `run`'s
#'    maintenance period
#' * `end`: End time of the event relative to the beginning of the `run`'s
#'    maintenance period
#' * `time`: Total elapsed time of the event
#' * `concurrency`: A number of events that are being processed concurrently
#' * `maintenance`: A boolean determining whether or not all simulated users
#'    are executing a session
#' * `label`: A human readable event name
#' * `json`: The parsed JSON provided in the `recording.log` file. If the field
#'    `message` exists, a `message_parsed` field is added containing a parsed
#'    form of the SockJS's JSON message content.
#'
#' @param ...  Key-value pairs where the key is the desired name for the test and the
#'   value is a path to the test result directory.
#' @param verbose Whether or not to print progress for reading loadtest directories
#' @eval roxygen_gen_df_desc("@return A tidy data frame with the test result data. Each row is an event. Columns include identifiers and timing information for the event. The variables are as follows", TRUE)
#' @export
#' @examples
#' \dontrun{
#' load_runs(
#'   `1 core` = "results/run-1/",
#'   `2 cores` = "results/run-2/"
#' )
#' }
load_runs <- function(..., verbose = vroom::vroom_progress()) {
  # TODO: Validate input directories and fail intelligently!
  verbose <- isTRUE(verbose)

  run_dirs <- list(...)
  if (length(names(run_dirs)) == 0) {
    names(run_dirs) <- vapply(run_dirs, basename, character(1))
  } else {
    is_missing_name <- names(run_dirs) == ""
    if (any(is_missing_name)) {
      names(run_dirs)[is_missing_name] <- vapply(run_dirs[is_missing_name], basename, character(1))
    }
  }

  first_recording <- list()

  df <- run_dirs %>%
    {
      mapply(
        ., names(.),
        USE.NAMES = FALSE, SIMPLIFY = FALSE,
        FUN = function(recording_path, run) {
          raw <- read_log_dir(file.path(recording_path, "sessions"), run, verbose = verbose)
          df_run <- get_times(raw)

          recording_path <- file.path(recording_path, "recording.log")
          if (is.null(first_recording$name)) {
            first_recording$name <<- run
            first_recording$lines <<- readLines(recording_path)
            recording <- read_recording(recording_path)
            first_recording$data <<- recording %>% select(input_line_number, label, json)
            first_recording$max_end <<- max(recording$end)
          } else {
            recording <- readLines(recording_path)
            if (!identical(recording, first_recording$lines)) {
              cli::cli_abort(paste0(
                "Recording for `", run, "` does not equal the recording for `", first_recording$name, "`.\n",
                "Please use the same recording when calling `load_runs()`",
                call = NULL
              ))
            }
          }
          df_recording <- first_recording$data

          df_run$id <- seq_len(nrow(df_run))
          df_maintenance_ids <- maintenance_df_ids(df_run)

          df_run <- df_run %>%
            mutate(maintenance = id %in% df_maintenance_ids) %>%
            select(-id)

          min_maintenance_time <- df_run %>%
            filter(maintenance == TRUE) %>%
            magrittr::extract2("start") %>%
            min()
          if (!is.infinite(min_maintenance_time)) {
            df_run <- df_run %>%
              mutate(
                start = start - min_maintenance_time,
                end = end - min_maintenance_time
              )
          }

          left_join(df_run, df_recording, by = "input_line_number")
        }
      )
    } %>%
    bind_rows() %>%
    mutate(
      run = factor(run, names(run_dirs), ordered = TRUE)
    )

  attr(df, "recording_duration") <- max(first_recording$max_end)
  df
}



maintenance_df_ids <- function(df) {
  lapply(
    unique(df$run),
    function(runVal) {
      df_run <- df %>% filter(run == runVal)

      # if there is only one iteration... return the whole data frame
      if (isTRUE(all.equal(max(df_run$iteration), 0))) {
        return(df_run$id)
      }

      # for each 'user', get the max(first start) and min(last end)
      df_time <- df %>%
        group_by(user_id) %>%
        summarise(
          start_time = min(start),
          end_time = max(end)
        )
      start_time <- max(df_time$start_time)
      end_time <- min(df_time$end_time)

      df_times <- df_run %>%
        group_by(session_id) %>%
        summarise(min_start = min(start), max_end = max(end)) %>%
        filter(min_start >= start_time, max_end <= end_time)

      ret <- df_run %>%
        ungroup() %>%
        filter(session_id %in% df_times$session_id)
      if (nrow(ret) == 0) {
        message(
          "  Could not find a valid maintenance period.\n",
          "  Make sure at least one complete iteration exists between the latest user start time and the earliest user end time.\n",
          "  Setting the maintenance period to be whole run."
        )
        return(df_run$id)
      }
      ret$id
    }
  ) %>%
    unlist()
}
