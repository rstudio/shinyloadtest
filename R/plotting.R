# Plotting functions ------------------------------------------------------


#' Boxplot of Event Duration
#'
#' @param df A tidy data frame of test results. See \code{\link{analyze_runs}}
#' @param labels A vector of labels to include
#' @import ggplot2
#' @export
plot_time_boxplot <- function(df, labels = NULL) {
  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% dplyr::filter(label %in% UQ(labels))
  }
  p <- df %>%
    ggplot(aes(run, time, fill = run)) +
    geom_boxplot() +
    facet_wrap(~label) +
    scale_fill_brewer(type = "qual")
  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p
}


#' Concurrency Over Time
#'
#' @param df A tidy data frame of test results. See \code{\link{analyze_runs}}
#' @param labels A vector of labels to include
#'
#' @import ggplot2
#' @export
plot_concurrency_time <- function(df, labels = NULL) {
  if (!is.null(labels)) {
    labels <- enexpr(labels)
    df <- df %>% dplyr::filter(label %in% UQ(labels))
  }
  p <- df %>%
    ggplot(aes(concurrency, time, color = run)) +
    geom_point() +
    geom_smooth()

  if(is.null(labels) || length(labels) > 1) {
    p <- p + facet_wrap(~label)
  }
  p
}


#' Histogram of Page Load Times
#'
#' @param df A tidy data frame of test results. See \code{\link{analyze_runs}}
#' @param max_load_time The amount of time users will wait for the page to load
#'   when first requesting the app.
#' @import ggplot2
#' @export
hist_loadtimes <- function(df, max_load_time = 5) {
  p <- df %>%
    dplyr::group_by(run, session) %>%
    dplyr::summarise(begin = min(start), ready = start[event == "WS_OPEN"], finish = max(end)) %>%
    ggplot(aes(ready - begin)) +
    geom_histogram() +
    geom_vline(xintercept = max_load_time, color = "red") +
    xlab("Initial Page Load Time (sec)") +
    ylab("Sessions")

  if (length(levels(df$run)) > 1) {
    p <- p + facet_wrap(~run)
  }
  p
}

