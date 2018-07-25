if (getRversion() >= "2.15.1") {
  # TODO remove and upgrade the dplyr fns to FN_()
  utils::globalVariables(c("input_line_number", "run", "session_id", "worker_id", "iteration", "event", "timestamp", "concurrency", "center", "event_class", "total_latency", ".", "type"))
}

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
  df
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
    mutate(concurrency = cumsum(relative_concurrency),
           run = name)

  df
}

# Read a recording fil
read_recording <- function(fileName) {
  baselineInfo <- readLines(fileName) %>%
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
      )
    }) %>%
    bind_rows() %>%
    mutate(
      session_id = -1,
      worker_id = -1,
      iteration = -1,
      input_line_number = seq_len(nrow(.)),
      time = end - start,
      concurrency = 1,
      label = paste0(input_line_number, ":", event)
    ) %>%
    select(
      session_id, worker_id, iteration,
      input_line_number,
      event,
      start, end, time,
      concurrency,
      label
    )
}

get_times <- function(df) {
  df %>%
    filter(!is.na(input_line_number)) %>%
    group_by(run, session_id, worker_id, iteration, input_line_number) %>%
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
#' @param baselineName string matching a name of \code{...}. Defaults to the first name.
#'
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
tidy_loadtest <- function(..., baselineName = names(list(...))[1]) {

  # TODO: Validate input directories and fail intelligently!

  run_levels <- names(list(...))

  if (!all(baselineName %in% run_levels)) {
    notFound <- baselineName[baselineName %in% run_levels]
    message(
      "baselineName: ", notFound, "' not found in names of shinycannon output supplied.\n",
      "Supplied: ", paste(baselineName, collapse = "', '"), "\n",
      "Available: ", paste(run_levels, collapse = "', '")
    )
  }

  df <- list(...) %>%
    lapply(file.path, "workers") %>%
    { mapply(., FUN = read_log_dir, names(.), USE.NAMES = FALSE, SIMPLIFY = FALSE) } %>%
    lapply(get_times) %>%
    bind_rows() %>%
    filter(event != "PLAYBACK_SLEEPBEFORE", event != "PLAYER_SESSION") %>%
    arrange(input_line_number, run) %>%
    mutate(
      label = paste0(input_line_number, ":", event),
      type = "record"
    ) %>%
    select(run, type, everything())

  # dfBaseline <- list(...) %>%
  #   lapply(file.path, "recording.log") %>%
  #   {
  #     mapply(
  #       ., names(.),
  #       USE.NAMES = FALSE, SIMPLIFY = FALSE,
  #       FUN = function(recordingPath, run) {
  #         read_recording(recordingPath) %>%
  #           mutate(
  #             run = run,
  #             type = "baseline"
  #           )
  #       }
  #     )
  #   } %>%
  #   bind_rows() %>%
  #   select(run, type, everything())
  #
  # df <- bind_rows(df, dfBaseline)

  fct_levels <- df$input_line_number[!duplicated(df$input_line_number)]
  fct_labels <- df$label[!duplicated(df$input_line_number)]
  df <- df %>%
    mutate(
      label = factor(input_line_number, fct_levels, fct_labels, ordered = TRUE),
      run = factor(run, run_levels, ordered = TRUE),
      baseline = factor(ifelse(run %in% baselineName, "baseline", "test"), c("baseline", "test"), ordered = TRUE)
    )

  df
}




# Plotting functions
#' Analysis plots
#'
#' @param df dataframe returned from \code{\link{tidy_loadtest}}
#' @export
#' @rdname analysis_plots
plot_concurrency_time_by_eventtype <- function(df) {
  df %>%
    mutate(event = ifelse(event == "REQ_HOME", "REQ", event)) %>%
    ggplot(aes(concurrency, time, color = run)) +
    geom_point() +
    facet_wrap(~event)
}

#' @export
#' @rdname analysis_plots
plot_gantt <- function(df) {
  df %>%
    filter(event != "WS_RECV_INIT", event != "WS_CLOSE") %>%
    mutate(worker_id = factor(worker_id, levels = rev(unique(worker_id)))) %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    ggplot(aes(x = center, y = worker_id, width = (end - start), fill = event)) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(baseline, run), scales="free_y", space="free_y") +
      # scale_fill_viridis_c() +
      scale_y_discrete(labels = {
        rev(unique(df$worker_id))
      }, breaks = {
        rev(unique(df$worker_id))
      }) +
      labs(
        x = "Elapsed time (sec)",
        y = "Session #",
        subtitle = "smaller bar width is better"
      ) +
      theme(legend.position = "bottom")
}
#' @export
#' @rdname analysis_plots
plot_gantt_session <- function(df) {
  df %>%
    filter(event != "WS_RECV_INIT") %>%
    mutate(worker_id = factor(worker_id, levels = rev(unique(worker_id)))) %>%
    mutate(session_id = factor(session_id, levels = rev(unique(session_id)))) %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    ggplot(aes(x = center, y = session_id, width = (end - start), fill = event)) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(baseline, run), scales="free_y", space="free_y") +
      # scale_fill_viridis_c() +
      scale_y_discrete(labels = {
        rev(unique(df$session_id))[seq_along(unique(df$session_id)) %% 5 == 0]
      }, breaks = {
        rev(unique(df$session_id))[seq_along(unique(df$session_id)) %% 5 == 0]
      }) +
      ylab("Session #") +
      xlab("Elapsed time (sec)") +
      theme(legend.position = "bottom")
}
#' @export
#' @rdname analysis_plots
plot_gantt_iteration <- function(df) {
  df %>%
    filter(event != "WS_RECV_INIT") %>%
    mutate(worker_id = factor(worker_id, levels = rev(unique(worker_id)))) %>%
    mutate(session_id = factor(session_id, levels = rev(unique(session_id)))) %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    ggplot(aes(x = center, y = worker_id, width = (end - start),
      # fill = (iteration %% 2 == 0)
      fill = as.factor(iteration)
    )) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(baseline, run), scales="free_y", space="free_y") +
      # scale_fill_discrete("iteration", breaks = c(TRUE, FALSE), labels = c("even", "odd")) +
      # scale_fill_viridis_c() +
      scale_y_discrete(labels = {
        rev(unique(df$worker_id))
      }, breaks = {
        rev(unique(df$worker_id))
      }) +
      ylab("Session #") +
      xlab("Elapsed time (sec)") +
      theme(legend.position = "bottom")
}


#' @export
#' @rdname analysis_plots
plot_gantt_duration <- function(df) {
  df1 <- df %>%
    filter(event != "WS_RECV_INIT") %>%
    group_by(run, session_id, worker_id, iteration) %>%
    mutate(end = end - min(start), start = start - min(start)) %>%
    ungroup()

  sessions <- df1 %>%
    filter(event != "WS_RECV_INIT") %>%
    group_by(run, session_id, worker_id, iteration) %>%
    summarise(max = max(end)) %>%
    arrange(run, desc(max)) %>%
    group_by(run) %>%
    mutate(order = 1:length(session_id)) %>%
    ungroup()

  df1 <- df1 %>%
    inner_join(sessions, by = c("run", "session_id", "worker_id", "iteration"))
  df1 %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    ggplot(aes(x = center, y = order, width = (end - start), fill = event)) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(baseline, run), scales="free_y", space="free_y") +
      # scale_fill_brewer(palette = "RdBu") +
      geom_vline(xintercept = 10, color = "yellow") +
      theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      labs(
        x = "Time since session start (sec)",
        y = "Sessions by duration",
        subtitle = "smaller bar width is better"
      ) +
      theme(legend.position = "bottom")
}

#' @export
#' @rdname analysis_plots
plot_gantt_latency <- function(df) {
  df %>%
    filter(event != "WS_RECV_INIT") %>%
    mutate(session_id = factor(session_id, levels = rev(unique(session_id)))) %>%
    mutate(event_class = c(REQ_HOME="REQ", REQ_GET="REQ", WS_OPEN="WS", WS_RECV="WS", WS_SEND="WS")[event]) %>%
    mutate(event = factor(event,
      levels = c("REQ", "WS"),
      labels = c("HTTP", "WebSocket"))) %>%
    group_by(baseline, run, session_id, event_class, worker_id) %>%
    summarise(total_latency = sum(time)) %>%
    ggplot(aes(rev(session_id), total_latency, fill = event_class, group = event_class)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      geom_text(aes(label = worker_id), color = "black", vjust = 0, position = position_stack(reverse = TRUE)) +
      # geom_step(position = position_stack(reverse = TRUE)) +
      facet_grid(rows = vars(baseline, run)) +
      scale_fill_manual(values = RColorBrewer::brewer.pal(4, "RdBu")[c(1,3)]) +
      scale_x_discrete(labels = {
        rev(unique(df$session_id)[seq_along(unique(df$session_id)) %% 5 == 1])
      }, breaks = {
        unique(df$session_id)[seq_along(unique(df$session_id)) %% 5 == 1]
      }) +
      labs(
        x = "Session",
        y = "Latency (sec)",
        subtitle = "shorter bar is better"
      ) +
      theme(legend.position = "bottom")
}

# plot_gantt_latency_stacked <- function(df) {
#
#   alter_data <- function(x) {
#     x %>%
#       filter(event != "WS_RECV_INIT") %>%
#       mutate(session_id = factor(session_id, levels = rev(unique(session_id)))) %>%
#       mutate(event_class = c(REQ_HOME="REQ", REQ_GET="REQ", WS_OPEN="WS", WS_RECV="WS", WS_SEND="WS")[event]) %>%
#       mutate(event = factor(event,
#         levels = c("REQ", "WS"),
#         labels = c("HTTP", "WebSocket"))) %>%
#       group_by(baseline, run, session_id, event_class, worker_id) %>%
#       summarise(total_latency = sum(time))
#   }
#
#   cols <- RColorBrewer::brewer.pal(4, "RdBu")[c(1,3)]
#   dfNonBaseline <- df %>%
#     filter(baseline != "baseline") %>%
#     alter_data() %>%
#     mutate(
#       .col = ifelse(event_class == "REQ", cols[1], cols[2])
#     )
#   dfBaseline <- df %>%
#     filter(baseline == "baseline") %>%
#     alter_data() %>%
#     ungroup() %>%
#     select(-run, -baseline) %>%
#     mutate(
#       .col = ifelse(event_class == "REQ", "darkgrey", "grey")
#     )
#
#   ggplot(data = dfNonBaseline, aes(session_id, total_latency, fill = I(.col), group = event_class)) +
#     geom_col(data = dfBaseline, position = position_stack(reverse = TRUE), show.legend = FALSE, col = "black") +
#     geom_col(position = position_stack(reverse = TRUE), alpha = 0.25, col = "grey") +
#     geom_text(aes(label = worker_id), color = "black", vjust = 0, position = position_stack(reverse = TRUE)) +
#     # geom_step(position = position_stack(reverse = TRUE)) +
#     facet_grid(rows = vars(baseline, run)) +
#     # scale_fill_manual(values = ) +
#     scale_x_discrete(labels = {
#       unique(df$session_id)[1:50 %% 5 == 1]
#     }, breaks = {
#       unique(df$session_id)[1:50 %% 5 == 1]
#     })
# }
