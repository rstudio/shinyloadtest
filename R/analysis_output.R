#' Make shinyloadtest Report
#'
#' @param df data.frame returned from \code{\link{tidy_loadtest}}
make_report <- function(df) {
  output_file = tempfile(fileext = ".Rmd")
  report <- glue_report(df)
  writeLines(report, output_file)
  output_file
}


glue_report <- function(df) {

  tmpFile <- tempfile()
  saveRDS(df, tmpFile)
  glue::glue_data(
    list(
      fileLocation = tmpFile
    ),
'

---
title: "shinyloadtest report"
runtime: shiny
output:
  flexdashboard::flex_dashboard:
    orientation: rows
    social: menu
---

```{{r, include=FALSE}}
base::library(shinyloadtest)
df <- readRDS("{fileLocation}")
```



# Sessions

## Row

### Worker activity over time

```{{r}}
shiny::renderPlot({{plot_gantt(df)}})
```

### Duration of each session

```{{r}}
shiny::renderPlot({{plot_gantt_duration(df)}})
```


# Time

## Row

### Page Load Time

```{{r}}
shiny::renderPlot({{hist_loadtimes_stacked(df)}})
```

### Elapsed Time Event Waterfall

```{{r}}
shiny::renderPlot({{plot_timeline(df)}})
```

### Latency per session

```{{r}}
shiny::renderPlot({{plot_gantt_latency(df)}})
```

# Event Duration

## Row

### Event duration within each run

```{{r}}
shiny::renderPlot({{plot_time_boxplot(df)}})
```

# Event Concurrency

## Row

### Event duration given concurrency for each run

```{{r}}
shiny::renderPlot({{plot_concurrency_time(df)}})
```

')

}
