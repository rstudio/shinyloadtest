if (getRversion() >= "2.15.1") {
  # TODO remove and upgrade the dplyr fns to FN_()
  utils::globalVariables(c("start", "end", "ready", "begin", "label", "time"))
}


# Plotting functions ------------------------------------------------------


#' Boxplot of Event Duration
#'
#' @param df A tidy data frame of test results. See \code{\link{tidy_loadtest}}
#' @param labels A vector of labels to include
#' @import ggplot2
#' @export
plot_time_boxplot <- function(df, labels = NULL) {
  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% filter(label %in% UQ(labels))
  }
  p <- df %>%
    ggplot(aes(run, time, fill = run)) +
    geom_boxplot() +
    facet_wrap(~label) +
    scale_fill_brewer(type = "qual") +
    labs(subtitle = "lower is better") +
    theme(legend.position = "bottom")
  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p
}


#' Concurrency Over Time
#'
#' @param df A tidy data frame of test results. See \code{\link{tidy_loadtest}}
#' @param labels A vector of labels to include
#'
#' @import ggplot2 dplyr
#' @export
plot_concurrency_time <- function(df, labels = NULL) {
  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% filter(label %in% UQ(labels))
  }
  p <- df %>%
    ggplot(aes(concurrency, time, color = run)) +
    geom_point() +
    geom_smooth() +
    labs(subtitle = "lower is better") +
    theme(legend.position = "bottom")

  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p + labs(subtitle = "lower is better")
}


#' @export
#' @rdname analysis_plots
plot_timestamp_time <- function(df) {
  df %>%
    ggplot(aes(end, time, color = run)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~label)
}

#' @export
#' @rdname analysis_plots
plot_timeline <- function(df) {
  df %>%
    # filter(session %% 5 == 1) %>%
    ggplot(aes(end, label, group = session_id, color = concurrency)) +
    geom_line(size = 1.2) +
    scale_color_viridis_c() +
    scale_y_discrete(limits = rev(levels(df$label))) +
    facet_grid(
      rows = vars(run)
    ) +
    labs(
      x = "Total elapsed time", y = NULL,
      subtitle = "vertical is better"
    ) +
    theme(legend.position = "bottom")
}
# ' @export
# ' @rdname analysis_plots
# plot_timeline_stacked <- function(df) {
#   dfNonBaseline <- df %>% filter(baseline != "baseline")
#   dfBaseline <- df %>% filter(baseline == "baseline")
#   dfNonBaseline %>%
#     # filter(session %% 5 == 1) %>%
#     ggplot(aes(end, label, group = session_id, color = concurrency)) +
#     geom_line(data = dfBaseline %>% select(-run), color = "grey", size = 1.2) +
#     geom_line(size = 1.2, alpha = 0.5) +
#     scale_color_viridis_c() +
#     scale_y_discrete(limits = rev(levels(df$label))) +
#     facet_grid(rows = vars(run), scales = "free_x", space = "free_x") +
#     labs(
#       x = "Total elapsed time", y = NULL,
#       subtitle = "vertical is better. baseline grey, in background"
#     ) +
#     theme(legend.position = "bottom")
# }



#' Histogram of Page Load Times
#'
#' @param df A tidy data frame of test results. See \code{\link{tidy_loadtest}}
#' @param max_load_time The amount of time users will wait for the page to load
#'   when first requesting the app.
#' @import ggplot2
#' @export
hist_loadtimes <- function(df, max_load_time = 5) {
  p <- df %>%
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
# ' @export
# ' @rdname hist_loadtimes
# hist_loadtimes_stacked <- function(df, max_load_time = 5) {
#   dfNonBaseline <- df %>%
#     filter(baseline != "baseline") %>%
#     group_by(run, session_id) %>%
#     summarise(begin = min(start), ready = start[event == "WS_OPEN"], finish = max(end))
#   dfBaseline <- df %>%
#     filter(baseline == "baseline") %>%
#     group_by(run, session_id) %>%
#     summarise(begin = min(start), ready = start[event == "WS_OPEN"], finish = max(end)) %>%
#     ungroup() %>%
#     select(-run)
#
#   p <- dfNonBaseline %>%
#     ggplot(aes(ready - begin, fill = run)) +
#     geom_histogram(data = dfBaseline, fill = "#252525", alpha = 0.5) +
#     geom_histogram(alpha = 0.5) +
#     geom_vline(xintercept = max_load_time, color = "red") +
#     labs(
#       x = "Inital Page Load Time (sec)",
#       y = "Session #",
#       subtitle = "shorter bar is better. baseline grey, in background"
#     ) +
#     theme(legend.position = "bottom")
#
#   if (length(levels(dfNonBaseline$run)) > 1) {
#     p <- p + facet_wrap(facets = vars(run))
#   }
#   p
# }
