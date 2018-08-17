

#' Make shinyloadtest Report
#'
#' @param df data.frame returned from \code{\link{tidy_loadtest}}
#' @param output File where HTML output should be saved
#' @param duration_cutoff Cutoff value for session duration plot
#' @param http_latency_cutoff Cutoff value for total http latency plot
#' @param max_websocket_cutoff Cutoff value for max websocket latency plot
#' @param verbose Boolean that determines if progress output is displayed
#' @export
make_report <- function(
  df,
  output = "test.html",
  duration_cutoff = 10,
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20,
  verbose = TRUE
) {
  if(!grepl(".html$", output)) {
    stop("'output' should end in '.html'")
  }

  if (verbose) {
    pr <- progress::progress_bar$new(
      format = ":evt [:bar] :current/:total eta::eta\n",
      total = 0 +
        1 + # move files
        3 + # time tab
        (length(levels(df$run)) * 2) + # per run plots
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
      if (n < total_n) {
        evt <- paste0(evt, paste0(rep(" ", total_n - n), collapse = ""))
      }
      pr$tick(tokens = list(evt = evt))
    }
  } else {
    tick <- identity
  }

  tick("Copy Files")
  # collect base path
  base_output_name <- sub(paste0(".", tools::file_ext(output)), "", output, fixed = TRUE)
  svg_folder <- "SVG"
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

  # helper function to save an svg
  save_svg <- function(p, file) {
    output <- file.path(base_output_name, svg_folder, paste0(file, ".svg"))
    if (file.exists(output)) return(output)
    suppressMessages({
      ggplot2::ggsave(filename = output, plot = p)
      # TODO make file smaller!
    })
    output
  }
  save_run_svg <- function(p, run, file) {
    save_svg(p, paste0(file, "-", run))
  }

  tick("Event Waterfall")
  src_timeline <- df %>%
    plot_timeline() %>%
    save_svg("timeline")

  tick("HTTP Latency")
  src_http <- df %>%
    plot_http_latency(cutoff = http_latency_cutoff) %>%
    save_svg("http_latency")

  tick("Websocket Latency")
  src_websocket <- df %>%
    plot_websocket_latency(cutoff = max_websocket_cutoff) %>%
    save_svg("websocket_latency")

  # gantt chart plots
  max_duration <- max(gantt_duration_data(df)$end)
  gantt <- lapply(levels(df$run), function(run_val) {
    run_val_clean <- run_val %>% tolower() %>% gsub("[^a-z0-9]", "-", .) %>% paste0("run-", .)
    df_run <- df %>% dplyr::filter(run == run_val)
    tick("Session Gantt")
    src_gantt <- df_run %>%
      plot_gantt() %>%
      save_run_svg(run_val_clean, "gantt")
    tick("Session Duration")
    src_duration <- {
        plot_gantt_duration(df_run, cutoff = duration_cutoff) + ggplot2::xlim(0, max_duration)
      } %>%
      save_run_svg(run_val_clean, "duration")
    list(
      n = length(unique(df_run$user_id)),
      id = run_val_clean,
      id_gantt = paste0(run_val_clean, "-gantt"),
      id_duration = paste0(run_val_clean, "-duration"),
      name = run_val,
      src_gantt = src_gantt,
      src_duration = src_duration
    )
  })

  # event boxplots
  boxplot <- lapply(sort(unique(df$input_line_number)), function(input_line_val) {
    df_event <- df %>% dplyr::filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Boxplot"))
    src_boxplot <- df_event %>%
      plot_time_boxplot() %>%
      save_run_svg(input_line_val, "boxplot")
    list(src = src_boxplot)
  })

  # event concurrency
  concurrency <- lapply(sort(unique(df$input_line_number)), function(input_line_val) {
    df_event <- df %>% dplyr::filter(input_line_number == input_line_val)
    tick(paste0("Event ", input_line_val, " Concurrency"))
    src_boxplot <- df_event %>%
      plot_concurrency_time() %>%
      save_run_svg(input_line_val, "concurrency")
    list(src = src_boxplot)
  })


  tick("Generate HTML")
  output_txt <- glue_index(list(
    base_output_name = base_output_name,
    src_timeline = src_timeline,
    src_http = src_http,
    src_websocket = src_websocket,
    gantt = gantt,
    boxplot = boxplot,
    concurrency = concurrency
  ))

  tick("Save HTML")
  writeLines(output_txt, output)

  tick("Done!")
  invisible(output)
}
