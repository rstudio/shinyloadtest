if (getRversion() >= "2.15.1") {
  # TODO remove and upgrade the dplyr fns to FN_()
  utils::globalVariables(c("max_error", "min_time", "max_time", "mean_time", "lm", "model", "coef", "residuals", "slope", "intercept", "slope_pos", "intercept_pos", "max_error_pos"))
}


#' Make shinyloadtest Report
#'
#' @param df data.frame returned from [load_runs()]
#' @param output File where HTML output should be saved
#' @param duration_cutoff Cutoff value for session duration plot. Defaults to the recording duration used to simulate `df` or 60 seconds.
#' @param http_latency_cutoff Cutoff value for total http latency plot
#' @param max_websocket_cutoff Cutoff value for max websocket latency plot
#' @param verbose Boolean that determines if progress output is displayed
#' @param self_contained Boolean that determines if the final output should be a self contained html file
#' @param open_browser Whether to open the created output in the browser
#' @return The path to the report, invisibly
#' @examples
#' \dontrun{
#'   shinyloadtest_report(slt_demo_data_1)
#' }
#' @export
shinyloadtest_report <- function(
  df,
  output = "shinyloadtest_report.html",
  duration_cutoff = c(attr(df, "recording_duration"), 60)[1],
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20,
  open_browser = TRUE,
  self_contained = TRUE,
  verbose = TRUE
) {
  if(!grepl(".html$", output)) {
    stop("'output' should end in '.html'", call. = FALSE)
  }

  verbose <- isTRUE(verbose)
  self_contained <- isTRUE(self_contained)
  missing_duration_cutoff <- missing(duration_cutoff)

  if (self_contained) {
    if (rmarkdown::pandoc_version() < "2.2") {
      stop("Please upgrade your pandoc version to be at least v2.2", call. = FALSE)
    }
  }

  if (verbose) {
    pr <- progress::progress_bar$new(
      format = ":name - :evt [:bar] :current/:total eta::eta",
      total = 0 +
        1 + # move files
        2 + # time tab
        (length(levels(df$run)) * 3) + # per run plots
        length(unique(df$input_line_number)) + # per event boxplot
        1 + # concurrency legend
        length(unique(df$input_line_number)) + # per event concurrency
        1 + # generate html
        1 + # save html
        1, # done
      show_after = 0,
      clear = FALSE
    )
    tick <- function(evt) {
      n <- nchar(evt)
      total_n <- 21
      if (n < total_n && FALSE) {
        evt <- paste0(evt, paste0(rep(" ", total_n - n), collapse = ""))
      }
      pr$tick(tokens = list(evt = evt, name = basename(base_output_name)))
    }
  } else {
    tick <- identity
  }

  # collect base path
  if (self_contained) {
    base_output_name <- file.path(
      tempdir(),
      paste0("shinyloadtest", floor(stats::runif(1, 1, 10000))),
      basename(sub(paste0(".", tools::file_ext(output)), "", output, fixed = TRUE))
    )
  } else {
    base_output_name <- sub(paste0(".", tools::file_ext(output)), "", output, fixed = TRUE)
  }
  tick("Copy Files")
  svg_folder <- "svg"
  # make sure output location exists
  dir.create(
    file.path(base_output_name, svg_folder),
    recursive = TRUE, showWarnings = FALSE
  )

  # copy js and css files
  c("js", "css") %>%
    file.path("dist", .) %>%
    system.file(package = "shinyloadtest") %>%
    file.copy(
      file.path(base_output_name),
      recursive = TRUE,
      overwrite = TRUE
    )

  save_run_svg <- function(p, run, file, ...) {
    save_svg_file(p, paste0(file, "-", run), ...)
  }
  save_svg_file <- function(p, file, ...) {
    output <- file.path(base_output_name, svg_folder, paste0(file, ".svg"))
    save_svg(p, output, ...)
    file.path(basename(base_output_name), svg_folder, paste0(file, ".svg"))
  }

  df_maintenance <- df %>% filter(maintenance == TRUE)

  latency_height <- 10 * (
      (
        (210.61 + 5.48) * length(unique(df$run))
      ) + 71.73
    ) / 720
  tick("HTTP Latency")
  src_http <- df %>%
    slt_http_latency(cutoff = http_latency_cutoff) %>%
    save_svg_file("http_latency", width = 15, height = latency_height)

  tick("Websocket Latency")
  suppressWarnings({
    src_websocket <- df %>%
      slt_websocket_latency(cutoff = max_websocket_cutoff) %>%
      save_svg_file("websocket_latency", width = 15, height = latency_height)
  })

  # gantt chart plots
  min_gantt_time <- min(df$start)
  max_gantt_time <- max(df$end)
  max_duration <- max(c(gantt_duration_data(df)$end, duration_cutoff))
  gantt <- lapply(levels(df$run), function(run_val) {
    run_val_clean <-
      run_val %>%
      tolower() %>%
      gsub("[^a-z0-9]", "-", .) %>%
      paste0("run-", .)
    df_run <-
      df %>%
      filter(run == run_val)

    tick(paste0(run_val, " Session Gantt"))
    src_gantt <- {
        slt_user(df_run) + xlim(min_gantt_time, max_gantt_time)
      } %>%
      save_run_svg(run_val_clean, "gantt", height = 7 * (26.625 * length(unique(df_run$user_id)) + 78) / 504)

    tick(paste0(run_val, " Session Duration"))
    src_duration <- {
        slt_session_duration(df_run, cutoff = duration_cutoff) + xlim(0, max_duration)
      } %>%
      save_run_svg(run_val_clean, "duration")

    tick(paste0(run_val, " Event Waterfall"))
    src_waterfall <- {
        slt_waterfall(df_run, limits = c(0, max(df_maintenance$concurrency, na.rm = TRUE))) + xlim(min_gantt_time, max_gantt_time)
      } %>%
      save_run_svg(
        run_val_clean,
        "waterfall",
        width = 15,
        height = 7 * (
          (
            340 / 37 * length(levels(df$label)) +
            5.48
          ) +
          86.17
        ) / 430
      )

    list(
      n = length(unique(df_run$user_id)),
      n_session = length(unique(df_run$session_id)),
      missing_duration_cutoff = missing_duration_cutoff,
      duration_cutoff = duration_cutoff,
      id = run_val_clean,
      id_gantt = paste0(run_val_clean, "-gantt"),
      id_duration = paste0(run_val_clean, "-duration"),
      id_waterfall = paste0(run_val_clean, "-waterfall"),
      name = run_val,
      src_gantt = src_gantt,
      src_duration = src_duration,
      src_waterfall = src_waterfall,
      idx = which(levels(df$run) %in% run_val)
    )
  })

  df_boxplot <- df_maintenance %>%
    group_by(label, run, input_line_number) %>%
    summarise(
      min_time = min(time, na.rm = TRUE),
      mean_time = mean(time, na.rm = TRUE),
      max_time = max(time, na.rm = TRUE)
    ) %>%
    group_by(label, input_line_number) %>%
    summarise(
      min_time = min(min_time, na.rm = TRUE),
      max_time = max(max_time, na.rm = TRUE),
      mean_diff = diff(range(mean_time, na.rm = TRUE))
    ) %>%
    arrange(input_line_number) %>%
    ungroup()


  format_num <- function(x, ...) {
    if (is.infinite(x) && x < 0) return("")
    formatC(x, format = "f", digits = 3, ...)
  }

  # event boxplots
  has_mean_diff <- length(levels(df$run)) > 1
  boxplot <- lapply(df_boxplot$input_line_number, function(input_line_val) {
    df_event <- df %>% filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Boxplot"))
    src_boxplot <- df_event %>%
      slt_time_boxplot() %>%
      save_run_svg(input_line_val, "boxplot", height = 4, width = 4)
    df_boxplot_event <- df_boxplot %>% filter(input_line_number == input_line_val)
    list(
      label = htmltools::htmlEscape(df_boxplot_event$label[[1]]),
      src = src_boxplot,
      has_mean_diff = has_mean_diff,
      min_time_val = df_boxplot_event$min_time[[1]] %>% format_num(),
      max_time_val = df_boxplot_event$max_time[[1]] %>% format_num(),
      mean_diff_val = df_boxplot_event$mean_diff[[1]] %>% format_num(),
      min_time = df_boxplot_event$min_time[[1]],
      max_time = df_boxplot_event$max_time[[1]],
      mean_diff = df_boxplot_event$mean_diff[[1]]
    )
  })

  df_model <- df_maintenance %>%
    group_by(label, run, input_line_number) %>%
    summarise(
      model = list(lm(time ~ concurrency))
    ) %>%
    mutate(
      slope = vapply(model, function(mod){ coef(mod)[2] }, numeric(1)),
      intercept = vapply(model, function(mod){ coef(mod)[1] }, numeric(1)),
      max_error = vapply(model, function(mod){ max(abs(c(residuals(mod), 0)), na.rm = TRUE) }, numeric(1)),
    ) %>%
    group_by(label, input_line_number) %>%
    summarise(
      slope_pos = which.max(c(abs(slope), -Inf)),
      slope_val = c(slope, -Inf)[slope_pos] %>% format_num(),
      slope = c(abs(slope), -Inf)[slope_pos],
      intercept_pos = which.max(c(abs(intercept), -Inf)),
      intercept_val = c(intercept, -Inf)[intercept_pos] %>% format_num(),
      intercept = c(abs(intercept), -Inf)[intercept_pos],
      max_error_pos = which.max(c(max_error, -Inf)),
      max_error_val = c(max_error, -Inf)[max_error_pos] %>% format_num(),
      max_error = c(max_error, -Inf)[max_error_pos]
    ) %>%
    arrange(input_line_number) %>%
    ungroup()

  # event concurrency
  concurrency <- lapply(df_model$input_line_number, function(input_line_val) {
    df_event <- df %>% filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Concurrency"))
    src_boxplot <- df_event %>%
      {
        slt_time_concurrency(.) + theme(legend.position = "none")
      } %>%
      save_run_svg(input_line_val, "concurrency", height = 4, width = 4)
    df_concurrency_event <- df_model %>% filter(input_line_number == input_line_val)
    list(
      label = htmltools::htmlEscape(df_concurrency_event$label[[1]]),
      src = src_boxplot,
      slope = df_concurrency_event$slope[[1]],
      intercept = df_concurrency_event$intercept[[1]],
      max_error = df_concurrency_event$max_error[[1]],
      slope_val = df_concurrency_event$slope_val[[1]],
      intercept_val = df_concurrency_event$intercept_val[[1]],
      max_error_val = df_concurrency_event$max_error_val[[1]]
    )
  })

  tick("Concurrency Legend")
  src_legend <- df %>%
    filter(input_line_number == df_model$input_line_number[[1]]) %>%
    {
      slt_time_concurrency(.) +
        theme(legend.position = "bottom") +
        labs(fill = "", color = "")
    } %>%
    extract_legend() %>%
    {
      legend_info  <- .
      # ratio <- legend_info$width_inches / legend_info$height_inches
      save_svg_file(
        legend_info$legend_grob,
        "concurrency_legend",
        height = legend_info$height_inches, width = legend_info$width_inches
      )
    }


  tick("Generate HTML")
  output_txt <- glue_index(list(
    folder_name = basename(base_output_name),
    src_http = src_http,
    src_websocket = src_websocket,
    src_legend = if (length(levels(df$run)) > 1) src_legend else NULL,
    gantt = gantt,
    run_length = length(levels(df$run)),
    http_latency_cutoff = http_latency_cutoff,
    max_websocket_cutoff = max_websocket_cutoff,
    boxplot = list(
      min_time = boxplot[order(df_boxplot$min_time, decreasing = TRUE)],
      max_time = boxplot[order(df_boxplot$max_time, decreasing = TRUE)],
      mean_diff = boxplot[order(df_boxplot$mean_diff, decreasing = TRUE)]
    ),
    concurrency = list(
      slope = concurrency[order(df_model$slope, decreasing = TRUE)],
      intercept = concurrency[order(df_model$intercept, decreasing = TRUE)],
      max_error = concurrency[order(df_model$max_error, decreasing = TRUE)]
    )
  ))

  tick("Saving HTML")

  if (self_contained) {
    tmp_file <- tempfile(tmpdir = dirname(base_output_name), fileext = ".html")
    writeLines(output_txt, tmp_file)

    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    cat("", file = output)
    output_path <- normalizePath(output)
    rmarkdown::pandoc_convert(input = tmp_file, output = output_path, options = c("--self-contained", "--template", tmp_file))
  } else {
    writeLines(output_txt, output)
  }


  tick(basename(output))
  if (open_browser) utils::browseURL(output)
  invisible(output)
}


# helper functions to save a ggplot2 to an svg
to_svgz <- function(in_path, out_path = tempfile()) {
  out <- gzfile(out_path, "w")
  writeLines(readLines(in_path), out)
  close(out)

  invisible(out_path)
}
save_svg <- function(p, output, width = 15, height = 10, units = "in", ...) {
  if (file.exists(output)) return(output)
  output_tmp <- tempfile(fileext = ".svg")
  on.exit({
    unlink(output_tmp)
  })
  suppressMessages({
    ggplot2::ggsave(
      filename = output_tmp,
      plot = p,
      width = width,
      height = height,
      limitsize = FALSE, # image size is programmatically increased and can be very tall
      ...
    )
    # TODO make file smaller!
  })
  # to_svgz(output_tmp, output)
  file.copy(output_tmp, output)
  output
}
