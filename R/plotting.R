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


#' Concurrency Over Time
#'
#' @param df A tidy data frame of test results. See \code{\link{tidy_loadtest}}
#' @param labels A vector of labels to include
#'
#' @import ggplot2 dplyr
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
    coord_cartesian(ylim = range(df$time))
    labs(subtitle = "lower is faster") +
    theme(legend.position = "bottom")

  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p + labs(subtitle = "lower is faster")
}


# #' @export
# #' @rdname analysis_plots
# plot_timestamp_time <- function(df) {
#   df %>%
#     ggplot(aes(end, time, color = run)) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~label)
# }

#' @export
#' @rdname analysis_plots
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
    geom_line(size = 1.2) +
    scale_color_viridis_c() +
    geom_rect(data = rect_df, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
    scale_fill_manual(NULL, values = request_colors()[["Warmup / Cooldown"]], limits = "Warmup / Cooldown") +
    scale_y_discrete(limits = rev(levels(df$label))) +
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


#' Histogram of Page Load Times
#'
#' @param df A tidy data frame of test results. See \code{\link{tidy_loadtest}}
#' @param max_load_time The amount of time users will wait for the page to load
#'   when first requesting the app.
#' @import ggplot2
#' @export
hist_loadtimes <- function(df, max_load_time = 5) {
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
