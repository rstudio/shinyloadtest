if (getRversion() >= "2.15.1") {
  # TODO remove and upgrade the dplyr fns to FN_()
  utils::globalVariables(c("start", "end", "ready", "begin", "label", "time"))
}

#' @import ggplot2 dplyr
NULL

# Plotting functions ------------------------------------------------------

#' Plotting outputs for tidy_loadtest
#'
#' Many different plotting routines to display different loadtest information.
#'
#' @name plot_loadtest
#' @param df data frame returned from \code{\link{tidy_loadtest}}
#' @param labels A vector of labels to include.  If none are supplied, all labels will be used.
#' @param cutoff Where to draw a horizontal or vertical line to display a resonable cutoff line for requests.
#' @param max_load_time The amount of time users will wait for the page to load
#'   when first requesting the app.
#' @rdname plot_loadtest
NULL


#' @describeIn plot_loadtest Box plot of load times for each event in each run
#' @export
plot_time_boxplot <- function(df, labels = NULL) {
  df <- df %>% filter(maintenance == TRUE)

  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% filter(label %in% UQ(labels))
  }

  p <- df %>%
    ggplot(aes(run, time, fill = run)) +
    geom_boxplot() +
    facet_wrap(~label) +
    scale_fill_brewer(type = "qual") +
    labs(subtitle = "lower is faster") +
    theme(legend.position = "bottom")

  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p
}


#' @describeIn plot_loadtest Time on concurrency for each event for each run
#' @export
plot_concurrency_time <- function(df, labels = NULL) {
  df <- df %>% filter(maintenance == TRUE)

  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% filter(label %in% UQ(labels))
  }

  p <- df %>%
    ggplot(aes(concurrency, time, color = run)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_fill_brewer(type = "qual") +
    coord_cartesian(ylim = range(df$time)) +
    labs(subtitle = "lower is faster") +
    theme(legend.position = "bottom")

  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p
}


# #' @export
# # plot_timestamp_time <- function(df) {
#   df %>%
#     ggplot(aes(end, time, color = run)) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~label)
# }

#' @describeIn plot_loadtest Event waterfall for each session within each run
#' @export
plot_timeline <- function(df) {
  non_maintenance <- df %>% filter(maintenance == FALSE) %>% as.data.frame()
  maintenance <- df %>% filter(maintenance == TRUE) %>% as.data.frame()
  rect_df <- data.frame(xmin = 0, xmax = 0, ymin = maintenance[1,"label"], ymax = maintenance[2, "label"], fill = "fill")
  maintenance %>%
    ggplot(
      aes(
        end, label,
        group = session_id,
        color = concurrency
      )
    ) +
    geom_line(data = non_maintenance, color = request_colors()[["Warmup / Cooldown"]], size = 1.2) +
    maintenance_vline_only(data = df, mapping = aes(xintercept = start), show.legend = FALSE) +
    maintenance_vline_only(data = df, mapping = aes(xintercept = end), show.legend = FALSE) +
    geom_rect(data = rect_df, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
    scale_fill_manual(request_scales_title, values = request_colors()[["Warmup / Cooldown"]], limits = "Warmup / Cooldown") +
    ## can not add due to competing color scales
    # request_scale_color(includeWarmup = TRUE) +
    # request_scale_guides() +
    scale_y_discrete(limits = rev(levels(df$label))) +
    geom_line(size = 1.2) +
    scale_color_viridis_c() +
    guides(
      color = guide_colorbar(order = 1),
      fill = guide_legend(order = 2)
    ) +
    facet_grid(
      rows = vars(run)
    ) +
    labs(
      x = "Total elapsed time", y = NULL,
      subtitle = "more vertical is faster"
    ) +
    theme(legend.position = "bottom")
}


#' @describeIn plot_loadtest Histogram of page load times
#' @export
plot_hist_loadtimes <- function(df, max_load_time = 5) {
  p <- df %>%
    filter(maintenance == TRUE) %>%
    group_by(run, session_id) %>%
    summarise(begin = min(start), ready = start[event == "WS_OPEN"], finish = max(end)) %>%
    ggplot(aes(ready - begin)) +
    geom_histogram() +
    geom_vline(xintercept = max_load_time, color = "red") +
    xlab("Initial Page Load Time (sec)") +
    ylab("Sessions") +
    theme(legend.position = "bottom")

  if (length(levels(df$run)) > 1) {
    p <- p + facet_grid(rows = vars(run))
  }
  p
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

maintenance_color <- "#2e2e2e"
maintenance_vline <- function(data, mapping, ...) {
  data <- data %>%
    group_by(run) %>%
    filter(maintenance == TRUE) %>%
    summarise(start = min(start), end = max(end), maintenance = "Warmup / Cooldown")

  data$maintenance = "Warmup / Cooldown"
  mapping$colour <- aes(color = maintenance)$colour
  geom_vline(data = data, mapping = mapping, size = 1, linetype = "dotted", ...)
}
maintenance_vline_only <- function(data, mapping, ...) {
  data <- data %>%
    group_by(run) %>%
    filter(maintenance == TRUE) %>%
    summarise(start = min(start), end = max(end), maintenance = "Warmup / Cooldown")

  geom_vline(data = data, mapping = mapping, size = 1, linetype = "dotted", color = maintenance_color, ...)
}
maintenance_session_vline <- function(data, mapping, ...) {
  data <- data %>%
    group_by(run) %>%
    filter(maintenance == TRUE) %>%
    summarise(start = min(as.numeric(session_id)) - 0.5, end = max(as.numeric(session_id)) + 0.5, maintenance = "Warmup / Cooldown")

  data$maintenance = "Warmup / Cooldown"
  mapping$colour <- aes(color = maintenance)$colour
  geom_vline(data = data, mapping = mapping, size = 1, linetype = "dotted", ...)
}

request_scales_title <- ""
request_scale_fill <- function(
  includeWarmup = FALSE,
  limits = c("Homepage", "JS/CSS", "Start session", "Calculate", if (includeWarmup) "Warmup / Cooldown")
) {
  includeWarmup <- isTRUE(includeWarmup)

  scale_fill_manual(request_scales_title, values = request_colors(), limits = limits)
}

request_scale_color <- function(
  includeWarmup = TRUE,
  limits = c("Homepage", "JS/CSS", "Start session", "Calculate", if (includeWarmup) "Warmup / Cooldown")
) {
  clear <- "transparent"
  values <- request_colors()
  for (key in names(values)) {
    values[[key]] <- clear
  }
  values[["Warmup / Cooldown"]] <- maintenance_color

  scale_color_manual(request_scales_title, limits = limits, values = values)
}
request_scale_guides <- function() {
  g <- guide_legend(request_scales_title)
  guides(fill = g, color = g)
}


#' @describeIn plot_loadtest Gantt chart of event duration for each session within each run
#' @export
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
    maintenance_vline(data = df_gantt, mapping = aes(xintercept = start)) +
    maintenance_vline(data = df_gantt, mapping = aes(xintercept = end)) +
    request_scale_fill(includeWarmup = TRUE) +
    request_scale_color(includeWarmup = TRUE) +
    request_scale_guides() +
    facet_grid(rows = vars(run), scales="free_y", space="free_y") +
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

#' @describeIn plot_loadtest Event gantt chart of each user session within each run
#' @export
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
      maintenance_vline(data = df_session, mapping = aes(xintercept = start)) +
      maintenance_vline(data = df_session, mapping = aes(xintercept = end)) +
      request_scale_fill(includeWarmup = TRUE) +
      request_scale_color(includeWarmup = TRUE) +
      request_scale_guides() +
      scale_y_discrete(labels = session_breaks, breaks = session_breaks) +
      ylab("Session #") +
      xlab("Elapsed time (sec)") +
      theme(legend.position = "bottom")
}


#' @describeIn plot_loadtest Event gantt chart of fastest to slowest session times within each run
#' @export
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
        NULL,
        values = request_colors(),
        limits = c("Homepage", "JS/CSS", "Start session", "Calculate")
      ) +
      geom_vline(xintercept = cutoff, color = cutoffColor) +
      theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
      labs(
        x = "Time since session start (sec)",
        y = "Sessions ordered by total duration",
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
    mutate(event = c(REQ_HOME="Homepage", REQ_GET="JS/CSS", WS_OPEN="Start session", WS_RECV="Calculate", WS_SEND="Calculate")[event]) %>%
    mutate(event = factor(event,
      levels = c("Homepage", "JS/CSS", "Start session", "Calculate"))) %>%
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

#' @describeIn plot_loadtest Stacked bar chart of event duration for each session within each run
#' @export
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
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = start)) +
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = end)) +
    request_scale_fill(includeWarmup = TRUE) +
    request_scale_color(includeWarmup = TRUE) +
    request_scale_guides() +
    facet_grid(rows = vars(run)) +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    labs(
      x = "Session",
      y = "Total Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}

#' @describeIn plot_loadtest Bar chart of total HTTP latency for each session within each run
#' @export
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
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = start)) +
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = end)) +
    request_scale_fill(includeWarmup = TRUE, limits = c("Homepage", "JS/CSS", "Warmup / Cooldown")) +
    request_scale_color(includeWarmup = TRUE, limits = c("Homepage", "JS/CSS", "Warmup / Cooldown")) +
    request_scale_guides() +
    # geom_step(position = position_stack(reverse = TRUE)) +
    facet_grid(rows = vars(run)) +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    geom_hline(yintercept = cutoff, color = cutoffColor) +
    labs(
      x = "Session",
      y = "Total HTTP Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}
#' @describeIn plot_loadtest Bar chart of maximum calculation (websocket) latency for each session within each run
#' @export
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
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = start)) +
    maintenance_session_vline(data = df_sum, mapping = aes(xintercept = end)) +
    request_scale_fill(includeWarmup = TRUE, limits = c("Calculate", "Warmup / Cooldown")) +
    request_scale_color(includeWarmup = TRUE, limits = c("Calculate", "Warmup / Cooldown")) +
    request_scale_guides() +
    scale_x_discrete(labels = session_breaks, breaks = session_breaks) +
    geom_hline(yintercept = cutoff, color = cutoffColor) +
    labs(
      x = "Session",
      y = "Maximum WebSocket Latency (sec)",
      subtitle = "shorter bar is faster"
    ) +
    theme(legend.position = "bottom")
}
