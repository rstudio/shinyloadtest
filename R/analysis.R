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
      user_id = -1,
      iteration = -1,
      input_line_number = seq_len(nrow(.)),
      time = end - start,
      concurrency = 1,
      label = paste0(input_line_number, ":", event)
    ) %>%
    select(
      session_id, user_id, iteration,
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
tidy_loadtest <- function(...) {

  # TODO: Validate input directories and fail intelligently!

  run_levels <- names(list(...))

  df <- list(...) %>%
    lapply(file.path, "workers") %>%
    { mapply(., FUN = read_log_dir, names(.), USE.NAMES = FALSE, SIMPLIFY = FALSE) } %>%
    lapply(get_times) %>%
    bind_rows() %>%
    filter(event != "PLAYBACK_SLEEPBEFORE", event != "PLAYER_SESSION") %>%
    arrange(run, user_id, session_id, input_line_number, ) %>%
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



request_color_column <- function(maintenance, event) {
  paste0(maintenance, "_", event)
}
request_colors <- function() {
  cols <- scales::hue_pal()(4)
  colors <- c("Homepage" = cols[1], "JS/CSS" = cols[2], "Start session" = cols[3], "Calculate" = cols[4])
  colorsMuted <- scales::muted(colors, 87, 10)
  colorsAll <- c(
    "Warmup / Cooldown" = "lightgrey",
    "Homepage" = colors[["Homepage"]],
    "JS/CSS" = colors[["JS/CSS"]],
    "Start session" = colors[["Start session"]],
    "Calculate" = colors[["Calculate"]],
    "TRUE_Homepage" = colors[["Homepage"]],
    "TRUE_JS/CSS" = colors[["JS/CSS"]],
    "TRUE_Start session" = colors[["Start session"]],
    "TRUE_Calculate" = colors[["Calculate"]],
    "FALSE_Homepage" = colorsMuted[["Homepage"]],
    "FALSE_JS/CSS" = colorsMuted[["JS/CSS"]],
    "FALSE_Start session" = colorsMuted[["Start session"]],
    "FALSE_Calculate" = colorsMuted[["Calculate"]]
  )
  colorsAll
}
# Plotting functions
#' Analysis plots
#'
#' @param df dataframe returned from \code{\link{tidy_loadtest}}
#' @export
#' @rdname analysis_plots
plot_concurrency_time_by_eventtype <- function(df) {
  df %>%
    filter(maintenance = TRUE) %>%
    mutate(event = ifelse(event == "REQ_HOME", "REQ", event)) %>%
    ggplot(aes(concurrency, time, color = run)) +
    geom_point() +
    facet_wrap(~event)
}

#' @export
#' @rdname analysis_plots
plot_gantt <- function(df) {
  df_gantt <- df %>%
    filter(event != "WS_RECV_INIT", event != "WS_CLOSE") %>%
    mutate(user_id = factor(user_id, levels = rev(unique(user_id)))) %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    mutate(colorCol = request_color_column(maintenance, event))


  ggplot(df_gantt, aes(x = center, y = user_id, width = (end - start), fill = colorCol)) +
    geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
    scale_fill_manual("Request", values = request_colors(), limits = c("Homepage", "JS/CSS", "Start session", "Calculate", "Warmup / Cooldown")) +
    facet_grid(rows = vars(run), scales="free_y", space="free_y") +
    # scale_fill_viridis_c() +
    scale_y_discrete(labels = {
      rev(unique(df$user_id))
    }, breaks = {
      rev(unique(df$user_id))
    }) +
    labs(
      x = "Elapsed time (sec)",
      y = "Session #",
      subtitle = "smaller bar width is faster"
    ) +
    theme(legend.position = "bottom")
}
#' @export
#' @rdname analysis_plots
plot_gantt_session <- function(df) {
  df_session <- df %>%
    filter(event != "WS_RECV_INIT") %>%
    mutate(user_id = factor(user_id, levels = sort(unique(user_id)))) %>%
    mutate(session_id = factor(session_id, levels = rev(sort(unique(session_id))))) %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    mutate(colorCol = request_color_column(maintenance, event))


  session_levels <- levels(df_session$session_id)
  if (length(session_levels) > 20) {
    session_breaks <- session_levels[seq_along(session_levels) %% 5 == 1]
  } else {
    session_breaks <- session_levels
  }


  df_session %>%
    ggplot(aes(x = center, y = session_id, width = (end - start), fill = colorCol)) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(run), scales="free_y", space="free_y") +
      scale_fill_manual(
        "Request",
        values = request_colors(),
        limits = c("Homepage", "JS/CSS", "Start session", "Calculate", "Warmup / Cooldown")
      ) +
      # scale_fill_viridis_c() +
      scale_y_discrete(labels = session_breaks, breaks = session_breaks) +
      ylab("Session #") +
      xlab("Elapsed time (sec)") +
      theme(legend.position = "bottom")
}


#' @export
#' @rdname analysis_plots
plot_gantt_duration <- function(df, cutoff = 10) {
  df1 <- df %>%
    filter(maintenance == TRUE) %>%
    filter(event != "WS_RECV_INIT") %>%
    group_by(run, session_id, user_id, iteration) %>%
    mutate(end = end - min(start), start = start - min(start)) %>%
    ungroup()

  sessions <- df1 %>%
    filter(event != "WS_RECV_INIT") %>%
    group_by(run, session_id, user_id, iteration) %>%
    summarise(max = max(end)) %>%
    arrange(run, desc(max)) %>%
    group_by(run) %>%
    mutate(order = 1:length(session_id)) %>%
    ungroup()

  df1 <- df1 %>%
    inner_join(sessions, by = c("run", "session_id", "user_id", "iteration"))
  df1 %>%
    mutate(center = (end + start) / 2) %>%
    mutate(event = factor(event,
      levels = c("REQ_HOME", "REQ_GET", "WS_OPEN", "WS_RECV", "WS_SEND"),
      labels = c("Homepage", "JS/CSS", "Start session", "Calculate", "WS_SEND"))) %>%
    ggplot(aes(x = center, y = order, width = (end - start), fill = event)) +
      geom_tile(height = 1, color = "#444444") + #, size = 0.3) +
      facet_grid(rows = vars(run), scales="free_y", space="free_y") +
      # scale_fill_brewer(palette = "RdBu") +
      scale_fill_manual(
        "Request",
        values = request_colors(),
        limits = c("Homepage", "JS/CSS", "Start session", "Calculate")
      ) +
      geom_vline(xintercept = cutoff, color = cutoffColor) +
      theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      labs(
        x = "Time since session start (sec)",
        y = "Sessions by duration",
        subtitle = "smaller bar width is faster"
      ) +
      theme(legend.position = "bottom")
}



latency_df <- function(df) {
  session_levels <- df$session_id %>% unique() %>% sort()
  df_sum <- df %>%
    filter(event != "WS_RECV_INIT") %>%
    # mutate(session_id = factor(session_id, levels = rev(unique(session_id)))) %>%
    mutate(user_id = paste0("w:", user_id)) %>%
    mutate(session_id = factor(session_id, levels = session_levels)) %>%
    mutate(event = c(REQ_HOME="Homepage", REQ_GET="JS/CSS", WS_OPEN="Calculate", WS_RECV="Calculate", WS_SEND="Calculate")[event]) %>%
    mutate(event = factor(event,
      levels = c("Homepage", "JS/CSS", "Calculate"))) %>%
    group_by(run, session_id, event, user_id, maintenance) %>%
    summarise(total_latency = sum(time), max_latency = max(time)) %>%
    mutate(colorCol = request_color_column(maintenance, event))
  df_sum
}

gantt_latency <- function(df) {
 df_lat <- latency_df(df) %>%
    spread(event, total_latency) %>%
    mutate(
      perc_http = HTTP / (HTTP + WebSocket)
    )
  info <- function(x, name) {
    data_frame(
      name = name,
      type = c(
        "mean",
        # "80%",
        "95%",
        "max"
      ),
      val = c(
        mean(x),
        # quantile(x, 0.8),
        quantile(x, 0.95),
        max(x)
      )
    )
  }

  fmt <- function(x, digits = 2, nsmall = 2) {
    format(x, digits = digits, nsmall = nsmall)
  }
  bind_rows(
    info(df_lat$HTTP, "HTTP") %>% mutate(val = fmt(val)),
    info(df_lat$Calculate, "Calculate") %>% mutate(val = fmt(val)),
    info(df_lat$perc_http, "percentage") %>% mutate(val = paste0(fmt(100 * val, 1, 1), "%")),
  ) %>%
    mutate(
      name = as.factor(name),
      type = factor(type, levels = c("mean", "95%", "max"))
    ) %>%
    spread(type, val)

}

#' @export
#' @rdname analysis_plots
plot_gantt_latency <- function(df) {
  df_sum <- latency_df(df)

  session_levels <- levels(df_sum$session_id)
  if (length(session_levels) > 20) {
    session_breaks <- session_levels[seq_along(session_levels) %% 5 == 1]
  } else {
    session_breaks <- session_levels
  }

  ggplot(
    df_sum,
    aes(session_id, max_latency, fill = colorCol, group = event)
  ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(
      "Request", values = request_colors(),
      limits = c("Homepage", "JS/CSS", "Calculate", "Warmup / Cooldown")
    ) +
    facet_grid(rows = vars(run)) +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    labs(
      x = "Session",
      y = "Total Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}

#' @export
#' @rdname analysis_plots
plot_http_latency <- function(df, cutoff = 10) {
  df_sum <- latency_df(df) %>%
    ungroup() %>%
    filter(event == "Homepage" | event == "JS/CSS") %>%
    mutate(event = factor(event, levels = c("Homepage", "JS/CSS"), ordered = TRUE))

  session_levels <- levels(df_sum$session_id)
  if (length(session_levels) > 20) {
    session_breaks <- session_levels[seq_along(session_levels) %% 5 == 1]
  } else {
    session_breaks <- session_levels
  }

  ggplot(
    df_sum,
    aes(session_id, total_latency, fill = colorCol, group = event)
  ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    # geom_step(position = position_stack(reverse = TRUE)) +
    facet_grid(rows = vars(run)) +
    scale_fill_manual("Request", values = request_colors(), limits = c("Homepage", "JS/CSS", "Warmup / Cooldown")) +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    geom_hline(yintercept = cutoff, color = cutoffColor) +
    labs(
      x = "Session",
      y = "Total HTTP Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}
#' @export
#' @rdname analysis_plots
plot_websocket_latency <- function(df, cutoff = 10) {
  df_sum <- latency_df(df) %>% filter(event == "Calculate")

  session_levels <- levels(df_sum$session_id)
  if (length(session_levels) > 20) {
    session_breaks <- session_levels[seq_along(session_levels) %% 5 == 1]
  } else {
    session_breaks <- session_levels
  }

  ggplot(
    df_sum,
    aes(session_id, max_latency, fill = colorCol, group = event)
  ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    facet_grid(rows = vars(run)) +
    scale_fill_manual("Request", values = request_colors(), limits = c("Calculate", "Warmup / Cooldown")) +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    geom_hline(yintercept = cutoff, color = cutoffColor) +
    labs(
      x = "Session",
      y = "Maximum WebSocket Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}
