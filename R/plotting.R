# TODO remove and upgrade the dplyr fns to FN_()
utils::globalVariables(c("start", "end", "ready", "begin", "label", "time", "maintenance", "quantile", "spread", "HTTP", "WebSocket", "val", "name", "max_latency", "colorCol", "xmin", "xmax", "ymin", "ymax", "fill"))

#' @rawNamespace import(ggplot2, except = vars)
#' @importFrom stats reorder
#' @import dplyr
NULL

# Plotting functions ------------------------------------------------------

#' Plotting outputs for shinyloadtest
#'
#' Many different plotting routines to display different loadtest information.
#'
#' @name slt_plot
#' @param df data frame returned from \code{\link{load_runs}}
#' @param labels A vector of labels to include.  If none are supplied, all labels will be used.
#' @param cutoff Where to draw a horizontal or vertical line to display a reasonable cutoff line for requests.
#' @param max_load_time The amount of time users will wait for the page to load
#'   when first requesting the app.
#' @rdname slt_plot
#' @return A \code{\link[ggplot2]{ggplot}} plot object
#' @examples
#' \donttest{
#' slt_user(slt_demo_data_4)
#' slt_session(slt_demo_data_4)
#' slt_session_duration(slt_demo_data_4)
#'
#' slt_waterfall(slt_demo_data_4)
#' slt_time_boxplot(slt_demo_data_4)
#' slt_time_concurrency(slt_demo_data_4)
#'
#' slt_session_latency(slt_demo_data_4)
#' slt_http_latency(slt_demo_data_4)
#' slt_websocket_latency(slt_demo_data_4)
#' slt_hist_loadtimes(slt_demo_data_4)
#' }
NULL

#' @describeIn slt_plot Box plot of load times for each event in each run
#' @export
slt_time_boxplot <- function(df, labels = NULL) {
  df <- df %>% filter(maintenance)

  if (!is.null(labels)) {
    df <- df %>% filter(label %in% !!labels)
  }

  df %>%
    ggplot(aes(run, time)) +
    geom_boxplot(aes(fill = run), show.legend = FALSE) +
    scale_fill_manual(NULL, values = run_fill_colors) +
    labs(
      y = "Time (sec)",
      x = NULL
    ) +
    theme(legend.position = "bottom") +
    facet_on_labels(df)
}


#' @describeIn slt_plot Time on concurrency for each event for each run
#' @export
slt_time_concurrency <- function(df, labels = NULL) {
  df <- df %>% filter(maintenance)

  if (!is.null(labels)) {
    df <- df %>% filter(label %in% !!labels)
  }

  df %>%
    ggplot(aes(concurrency, time)) +
    geom_smooth(
      aes(group = run),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      colour = "grey70"
    ) +
    geom_point(aes(colour = run)) +
    scale_colour_manual(NULL, values = run_fill_colors) +
    labs(
      x = "Concurrency",
      y = "Time (sec)"
    ) +
    theme(legend.position = "bottom") +
    facet_on_labels(df)
}

#' @describeIn slt_plot Event waterfall for each session within each run
#' @param limits passed into \code{\link[ggplot2]{scale_colour_gradientn}}
#' @export
slt_waterfall <- function(df, limits = NULL) {
  limits <- limits %||% c(0, NA)

  df1 <- df %>%
    filter(maintenance) %>%
    manip_event_label() %>%
    manip_session_relative() %>%
    mutate(label = fct_rev(str_trunc(label, 50)))

  ggplot(df1, aes(end, label, group = session_id, color = concurrency)) +
    geom_line(linewidth = 0.5) +
    maintenance_vline(df1, "time") +
    scale_colour_gradientn(
      colours = c("#413554", "#75aadb", "#9efa9e", "#fdc086"),
      limits = limits
    ) +
    labs(
      x = "Time since session start (sec)",
      y = NULL
    ) +
    theme(legend.position = "bottom") +
    facet_on_run(df1)
}

#' @describeIn slt_plot Histogram of page load times
#' @export
slt_hist_loadtimes <- function(df, max_load_time = 5) {
  df1 <- df %>%
    filter(maintenance) %>%
    group_by(run, session_id) %>%
    summarise(
      begin = min(start),
      ready = start[event == "WS_OPEN"],
      finish = max(end)
    )

  df1 %>%
    ggplot(aes(ready - begin)) +
    geom_histogram() +
    geom_vline(xintercept = max_load_time, color = "red") +
    labs(
      x = "Initial Page Load Time (sec)",
      y = "Number of sessions"
    ) +
    theme(legend.position = "bottom") +
    facet_on_run(df)
}

#' @describeIn slt_plot Gantt chart of event duration for each user within each run
#' @export
slt_user <- function(df) {
  df1 <- df %>%
    # Not filtering to maintenance as it is odd to see longer delay times near the startup for what appears to be no reason
    # filter(maintenance) %>%
    manip_event_label() %>%
    manip_tile_position() %>%
    mutate(user_id = fct_rev(user_id))

  ggplot(df1, aes(center, user_id)) +
    geom_tile(aes(fill = eventLabel, width = duration), height = 1) +
    maintenance_vline(df1, "time") +
    scale_fill_manual(NULL, values = event_colours) +
    labs(
      x = "Elapsed time (sec)",
      y = "Simulated user #"
    ) +
    theme(legend.position = "bottom") +
    facet_on_run_free(df1)
}

#' @describeIn slt_plot Event gantt chart of each user session within each run
#' @export
slt_session <- function(df) {
  df_session <- df %>%
    filter(maintenance) %>%
    manip_event_label() %>%
    manip_tile_position() %>%
    mutate(
      user_id = factor(user_id, levels = sort(unique(user_id))),
      session_id = factor(session_id, levels = rev(sort(unique(session_id)))),
    )

  ggplot(df_session, aes(center, session_id, width = duration)) +
    geom_tile(aes(fill = eventLabel), height = 1) +
    maintenance_vline(df_session, "time") +
    scale_y_discrete(breaks = session_breaks(df$session_id)) +
    scale_fill_manual(NULL, values = event_colours) +
    labs(
      x = "Elapsed time (sec)",
      y = "Session #"
    ) +
    theme(legend.position = "bottom") +
    facet_on_run_free(df_session)
}

#' @describeIn slt_plot Event gantt chart of fastest to slowest session times within each run
#' @export
slt_session_duration <- function(df, cutoff = NULL) {
  cutoff <- cutoff %||% attr(df, "recording_duration") %||% 60

  df1 <- df %>%
    filter(maintenance) %>%
    manip_session_relative() %>%
    manip_event_label() %>%
    manip_tile_position()

  ggplot(df1, aes(center, as.integer(reorder(session_id, end, max)))) +
    geom_tile(aes(width = duration, fill = eventLabel), height = 1) +
    geom_vline(xintercept = cutoff, color = cutoff_color) +
    scale_y_continuous(labels = NULL) +
    scale_fill_manual(NULL, values = event_colours) +
    labs(
      x = "Time since session start (sec)",
      y = "Sessions (ordered by total duration)"
    ) +
    theme(legend.position = "bottom") +
    facet_on_run_free(df1)
}

latency_df <- function(df) {
  df %>%
    filter(maintenance) %>%
    manip_event_label() %>%
    mutate(user_id = paste0("w:", user_id)) %>%
    group_by(run, session_id, eventLabel, user_id, maintenance) %>%
    summarise(
      total_latency = sum(time),
      max_latency = max(time),
      .groups = "drop"
    )
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

#' @describeIn slt_plot Stacked bar chart of event duration for each session within each run
#' @export
slt_session_latency <- function(df) {
  slt_latency(df, NULL, 10)
}

#' @describeIn slt_plot Bar chart of total HTTP latency for each session within each run
#' @export
slt_http_latency <- function(df, cutoff = 5) {
  slt_latency(df, c("Homepage", "JS/CSS"), cutoff = cutoff)
}
#' @describeIn slt_plot Bar chart of maximum calculation (websocket) latency for each session within each run
#' @export
slt_websocket_latency <- function(df, cutoff = 5) {
  slt_latency(df, "Calculate", cutoff = cutoff)
}

slt_latency <- function(df, events, cutoff = 5) {
  df_sum <- latency_df(df)
  if (!is.null(events)) {
    df_sum <- df_sum %>% filter(eventLabel %in% events)
  }
  df_sum <- df_sum %>%
    mutate(eventLabel = fct_rev(eventLabel))

  ggplot(df_sum, aes(session_id, total_latency)) +
    geom_col(aes(fill = eventLabel), width = 0.95) +
    geom_hline(yintercept = cutoff, color = cutoff_color) +
    maintenance_vline(df_sum, "session") +
    scale_fill_manual(NULL, values = event_colours) +
    scale_x_continuous(labels = NULL) +
    labs(
      x = "Session",
      y = "Total latency (sec)"
    ) +
    theme(legend.position = "bottom", legend.justification = "left") +
    facet_on_run_free(df_sum)
}

# Data manipulation helpers -----------------------------------------------

manip_event_label <- function(df) {
  labels <- c(
    "Homepage" = "REQ_HOME",
    "JS/CSS" = "REQ_GET",
    "Start session" = "WS_OPEN",
    "Calculate" = "WS_RECV"
  )

  df %>%
    filter(event %in% labels) %>%
    mutate(
      eventLabel = factor(event, levels = labels, labels = names(labels))
    )
}

manip_tile_position <- function(df) {
  df %>%
    mutate(
      center = (end + start) / 2,
      duration = end - start,
    )
}

manip_session_relative <- function(df) {
  df %>%
    group_by(run, session_id, user_id, iteration) %>%
    mutate(
      end = end - min(start),
      start = start - min(start),
    )
}


# Facetting helpers -------------------------------------------------------

facet_on_labels <- function(df) {
  n_labels <- length(unique(df[["labels"]]))

  if (n_labels == 1) {
    labs(title = df$label[[1]])
  } else {
    facet_wrap(~label)
  }
}

facet_on_run <- function(df, ...) {
  if (length(unique(df$run)) > 1) {
    facet_grid(rows = vars(run), ...)
  } else {
    facet_null()
  }
}

facet_on_run_free <- function(df, ...) {
  facet_on_run(df, ..., scales = "free_y", space = "free_y")
}


# Annotations -------------------------------------------------------------

maintenance_vline <- function(data, type = c("time", "session")) {
  type <- match.arg(type)

  summary <- data %>%
    group_by(run) %>%
    filter(maintenance) %>%
    reframe(
      event = "Warm up / Cooldown",
      time = if (type == "time") {
        c(min(start), max(end))
      } else {
        range(as.numeric(session_id)) + c(-0.5, 0.5)
      }
    )

  geom_vline(
    aes(xintercept = time),
    data = summary,
    linewidth = 0.5,
    linetype = "dashed",
    colour = alpha("black", 0.70)
  )
}

session_breaks <- function(x) {
  x <- sort(unique(x))

  len <- length(x)
  if (len <= 20) {
    return(x)
  }

  divisor <- 5
  while (floor(len / divisor) > 20) {
    divisor <- divisor + 5
  }

  x[seq_along(x) %% divisor == 1]
}

# Colours -----------------------------------------------------------------

cutoff_color <- "red"

# Run colors
run_fill_colors <- c(
  "#7fc97f", # medium green
  "#beaed4", # medium purple
  "#fdc086", # medium orange
  "#f28983", # medium red
  "#7ddbb6", # medium teal
  "#75aadb", # medium blue
  "#5d945d", # dark green
  "#9084a1", # dark purple
  "#c9996b", # dark orange
  "#bd5c57", # dark red
  "#5fa68a", # dark teal
  "#5981a6", # dark blue
  "#9efa9e", # bright green
  "#e5d1ff", # bright purple
  "#8df5cc", # bright teal
  "#88c6ff", # bright blue
  "#3d613d", # very dark green
  "#625a6e", # very dark purple
  "#967250", # very dark orange
  "#8a433f", # very dark red
  "#467362", # very dark teal
  "#3d5973" # very dark blue
)
run_accent_color_map <- c(
  # Accent colors only to be used as accents
  # e.g. I'm currently using these colors for the lines and dots in the box plots
  # for use with data colors: 01, 07, 13, 17
  green = "#144714",

  # for use with data colors: 02, 08, 14, 18
  purple = "#413554",

  # for use with data colors: 03, 09, 19
  orange = "#7a4920",

  # for use with data colors: 04, 10, 20
  red = "#5c1815",

  # for use with data colors: 05, 11, 15, 21
  teal = "#14593e",

  # for use with data colors: 06, 12, 16, 22
  blue = "#103659"
)

run_accent_colors <- run_accent_color_map[c(1:6, 1:6, 1:2, 5:6, 1:6)]

event_colours <- c(
  "Homepage" = "#f28983",
  "JS/CSS" = "#fdc086",
  "Start session" = "#9cffd9",
  "Calculate" = "#75aadb"
)

maintenance_color <- "#2e2e2e"
