

#' Make shinyloadtest Report
#'
#' @param df data.frame returned from \code{\link{tidy_loadtest}}
#' @param output File where HTML output should be saved
#' @param duration_cutoff Cutoff value for session duration plot
#' @param http_latency_cutoff Cutoff value for total http latency plot
#' @param max_websocket_cutoff Cutoff value for max websocket latency plot
#' @param verbose Boolean that determines if progress output is displayed
#' @export
#' @import rmarkdown
make_report <- function(
  df,
  output = "test.html",
  duration_cutoff = 10,
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20,
  open_browser = TRUE,
  verbose = TRUE
) {
  if(!grepl(".html$", output)) {
    stop("'output' should end in '.html'")
  }

  if (verbose) {
    pr <- progress::progress_bar$new(
      format = ":name - :evt [:bar] :current/:total eta::eta",
      total = 0 +
        1 + # move files
        2 + # time tab
        (length(levels(df$run)) * 3) + # per run plots
        length(unique(df$input_line_number)) + # per event boxplot
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
  base_output_name <- sub(paste0(".", tools::file_ext(output)), "", output, fixed = TRUE)
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
    output <- file.path(base_output_name, svg_folder, paste0(file, ".svgz"))
    save_svg(p, output, ...)
    file.path(basename(base_output_name), svg_folder, paste0(file, ".svgz"))
  }

  df_maintenance <- df %>% filter(maintenance == TRUE)

  latency_height <- 10 * (
      (
        (210.61 + 5.48) * length(unique(df$run))
      ) + 71.73
    ) / 720
  tick("HTTP Latency")
  src_http <- df %>%
    plot_http_latency(cutoff = http_latency_cutoff) %>%
    save_svg_file("http_latency", width = 15, height = latency_height)

  tick("Websocket Latency")
  src_websocket <- df %>%
    plot_websocket_latency(cutoff = max_websocket_cutoff) %>%
    save_svg_file("websocket_latency", width = 15, height = latency_height)

  # gantt chart plots
  min_gantt_time <- min(df$start)
  max_gantt_time <- max(df$end)
  max_duration <- max(gantt_duration_data(df)$end)
  gantt <- lapply(levels(df$run), function(run_val) {
    run_val_clean <- run_val %>% tolower() %>% gsub("[^a-z0-9]", "-", .) %>% paste0("run-", .)
    df_run <- df %>% filter(run == run_val)

    tick(paste0(run_val, " Session Gantt"))
    src_gantt <- {
        plot_gantt(df_run) + xlim(min_gantt_time, max_gantt_time)
      } %>%
      save_run_svg(run_val_clean, "gantt", height = 7 * (26.625 * length(unique(df_run$user_id)) + 78) / 504)

    tick(paste0(run_val, " Session Duration"))
    src_duration <- {
        plot_gantt_duration(df_run, cutoff = duration_cutoff) + xlim(0, max_duration)
      } %>%
      save_run_svg(run_val_clean, "duration")

    tick(paste0(run_val, " Event Waterfall"))
    src_waterfall <- {
        plot_timeline(df_run, limits = range(df_maintenance$concurrency, na.rm = TRUE)) + xlim(min_gantt_time, max_gantt_time)
      } %>%
      save_run_svg(
        run_val_clean,
        "waterfall",
        width = 15,
        height = 7 * (
          (
            340 / 37 * length(levels(df$recording_label)) +
            5.48
          ) +
          86.17
        ) / 430
      )

    list(
      n = length(unique(df_run$user_id)),
      id = run_val_clean,
      id_gantt = paste0(run_val_clean, "-gantt"),
      id_duration = paste0(run_val_clean, "-duration"),
      id_waterfall = paste0(run_val_clean, "-waterfall"),
      name = run_val,
      src_gantt = src_gantt,
      src_duration = src_duration,
      src_waterfall = src_waterfall
    )
  })

  df_boxplot <- df_maintenance %>%
    group_by(recording_label, run, input_line_number) %>%
    summarise(
      min_time = min(time),
      mean_time = mean(time),
      max_time = max(time)
    ) %>%
    group_by(recording_label, input_line_number) %>%
    summarise(
      min_time = min(min_time),
      max_time = max(max_time),
      mean_diff = diff(range(mean_time))
    ) %>%
    arrange(input_line_number)


  # event boxplots
  boxplot <- lapply(df_boxplot$input_line_number, function(input_line_val) {
    df_event <- df %>% filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Boxplot"))
    src_boxplot <- df_event %>%
      plot_time_boxplot() %>%
      save_run_svg(input_line_val, "boxplot", height = 4, width = 4)
    df_boxplot_event <- df_boxplot %>% filter(input_line_number == input_line_val)
    list(
      src = src_boxplot,
      min_time = df_boxplot_event$min_time[[1]],
      max_time = df_boxplot_event$max_time[[1]],
      mean_diff = df_boxplot_event$mean_diff[[1]])
  })

  df_model <- df_maintenance %>%
    group_by(recording_label, run, input_line_number) %>%
    summarise(
      model = list(lm(time ~ concurrency))
    ) %>%
    mutate(
      slope = vapply(model, function(mod){ abs(coef(mod)[2]) }, numeric(1)),
      intercept = vapply(model, function(mod){ abs(coef(mod)[1]) }, numeric(1)),
      max_error = vapply(model, function(mod){ max(c(abs(residuals(mod)), 0), na.rm = TRUE) }, numeric(1)),
    ) %>%
    group_by(recording_label, input_line_number) %>%
    summarise(
      slope = max(slope),
      intercept = max(intercept),
      max_error = max(max_error)
    ) %>%
    arrange(input_line_number)

  # event concurrency
  concurrency <- lapply(df_model$input_line_number, function(input_line_val) {
    df_event <- df %>% filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Concurrency"))
    src_boxplot <- df_event %>%
      plot_concurrency_time() %>%
      save_run_svg(input_line_val, "concurrency", height = 4, width = 4)
    df_concurrency_event <- df_model %>% filter(input_line_number == input_line_val)
    list(
      src = src_boxplot,
      slope = df_concurrency_event$slope[[1]],
      intercept = df_concurrency_event$intercept[[1]],
      max_error = df_concurrency_event$max_error[[1]]
    )
  })


  tick("Generate HTML")
  output_txt <- glue_index(list(
    folder_name = basename(base_output_name),
    src_http = src_http,
    src_websocket = src_websocket,
    gantt = gantt,
    run_length = length(levels(df$run)),
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

  tick("Save HTML")
  writeLines(output_txt, output)

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
save_svg <- function(p, output, width = 15, height = 10, ...) {
  # if (file.exists(output)) return(output)
  output_tmp <- tempfile(fileext = ".svg")
  on.exit({
    unlink(output_tmp)
  })
  suppressMessages({
    ggplot2::ggsave(filename = output_tmp, plot = p, width = width, height = height, ...)
    # TODO make file smaller!
  })
  to_svgz(output_tmp, output)
}
