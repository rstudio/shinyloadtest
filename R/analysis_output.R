#' Make shinyloadtest Report
#'
#' @param df data.frame returned from \code{\link{tidy_loadtest}}
#' @export
make_report <- function(
  df,
  duration_cutoff = 10,
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20
) {
  output_file = tempfile(fileext = ".Rmd")
  report <- glue_report(
    df,
    duration_cutoff = duration_cutoff,
    http_latency_cutoff = http_latency_cutoff,
    max_websocket_cutoff = max_websocket_cutoff
  )
  writeLines(report, output_file)
  output_file
}


glue_report <- function(
  df,
  duration_cutoff = 10,
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20
) {

  tmpFile <- tempfile()
  saveRDS(df, tmpFile)
  glue::glue_data(
    list(
      fileLocation = tmpFile,
      duration_cutoff = duration_cutoff,
      http_latency_cutoff = http_latency_cutoff,
      max_websocket_cutoff = max_websocket_cutoff
    ),
'

---
title: "shinyloadtest report"
runtime: shiny
output:
  flexdashboard::flex_dashboard:
    # theme: simplex
    orientation: rows
    # social: menu
---

```{{r, include=FALSE}}
library(shinyloadtest)
df <- readRDS("{fileLocation}")
```



# Sessions

## Row

### Session activity over time

```{{r}}
shiny::renderPlot({{plot_gantt(df)}})
```

### Duration of each session

```{{r}}
shiny::renderPlot({{plot_gantt_duration(df, cutoff = {duration_cutoff})}})
```


# Time

## Row

### Elapsed event time waterfall

```{{r}}
shiny::renderPlot({{plot_timeline(df)}})
```

### Total http latency per session

```{{r}}
shiny::renderPlot({{plot_http_latency(df, cutoff = {http_latency_cutoff})}})
```

### Maximum websocket latency per session

```{{r}}
shiny::renderPlot({{plot_websocket_latency(df, cutoff = {max_websocket_cutoff})}})
```

# Event Duration

## Inputs {{.sidebar data-width=500}}

```{{r}}
df_label_run <- df %>%
  dplyr::group_by(label, run) %>%
  dplyr::summarise(
    min_time = min(time),
    mean_time = mean(time),
    max_time = max(time)
  )
df_label <- df_label_run %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(
    min_time = min(min_time),
    max_time = max(max_time),
    mean_diff = diff(range(mean_time))
  ) %>%
  dplyr::arrange(desc(mean_diff))
# inputSelectize(levels(df$label))
```

```{{r}}
actionButton("durationMin", "5 slowest min time")
actionButton("durationMax", "5 slowest max time")
actionButton("durationMeanDiff", "5 largest mean time difference")

```
```{{r}}
DT::dataTableOutput("durationTable")
output$durationTable <- DT::renderDataTable({{
  DT::datatable(
    df_label,
    selection = list(selected = 1:5),
    rownames = FALSE,
    options = list(
      pageLength = 15,
      order = list(list(3, "desc")),
      columnDefs = list(list(targets = 1:3, searchable = FALSE))
    )
  ) %>%
    DT::formatRound("min_time", 3) %>%
    DT::formatRound("max_time", 3) %>%
    DT::formatRound("mean_diff", 3)
}})
durationProxy <- DT::dataTableProxy("durationTable")
observeEvent(input$durationMin, {{
  df_min <- df_label %>% dplyr::arrange(desc(min_time)) %>% head(5)
  durationProxy %>% DT::selectRows(which(df_label$label %in% df_min$label))
}})
observeEvent(input$durationMax, {{
  df_max <- df_label %>% dplyr::arrange(desc(max_time)) %>% head(5)
  durationProxy %>% DT::selectRows(which(df_label$label %in% df_max$label))
}})
observeEvent(input$durationMeanDiff, {{
  df_mean_diff <- df_label %>% dplyr::arrange(desc(mean_diff)) %>% head(5)
  durationProxy %>% DT::selectRows(which(df_label$label %in% df_mean_diff$label))
}})
```


## Row

### Event duration within each run

```{{r}}
shiny::renderPlot({{
  if (length(input$durationTable_rows_selected) > 0) {{
    labels_to_keep <- df_label$label[input$durationTable_rows_selected]
    df_sub <- dplyr::filter(df, label %in% labels_to_keep)
  }} else {{
    df_sub <- df
  }}

  plot_time_boxplot(df_sub)
}})
```

# Event Concurrency

## Row

### Event duration given concurrency for each run

```{{r}}
shiny::renderPlot({{plot_concurrency_time(df)}})
```

')

}
